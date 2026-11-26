library(tidyverse)

#Part 1
#1.1 Import
#Import of quarterly files
price_files <- list.files("data/raw", pattern = "^stock_prices_.*\\.csv$", full.names = TRUE)

raw_prices <- read_csv(price_files, id = "source_file", col_types = cols(.default = "c")) |>
  mutate(source_file = basename(source_file))

companies <- read_csv("data/raw/companies.csv", col_types = cols(.default = "c"))
benchmark <-  read_csv("data/raw/market_benchmark.csv", col_types = cols(.default = "c"))

#fx_daily <- read_csv("fx_daily.csv")
#This gives a warning about parsing issues, when opening it as a text file we see it uses semicolons instead of commas thus we need to use read_csv2
#read_csv2 also expects decimal comma; summary shows rates between 1 and 30 CZK, so numbers were read correctly
fx_daily <- read_csv2("data/raw/fx_daily.csv")
summary(fx_daily) 

#1.2 Investigation

table(raw_prices$source_file)
nrow(companies)
nrow(benchmark)
nrow(fx_daily)

sapply(raw_prices, function(x) sum(is.na(x)))
glimpse(raw_prices)

#price, volume and currency formats
raw_prices |> count(price_pattern = str_replace_all(price, "[0-9]", "9")) |> print(n = Inf)
raw_prices |> count(volume_pattern = str_replace_all(volume, "[0-9]", "9")) |> print(n = Inf)
raw_prices |> count(currency)
glimpse(companies)

glimpse(benchmark)
glimpse(fx_daily)
sapply(benchmark, function(x) sum(is.na(x)))
sapply(fx_daily, function(x) sum(is.na(x)))

raw_prices |> distinct(ticker) |> anti_join(companies, by = "ticker")

#Part 2
#2.1 Dates, prices and identifiers
#firstly we fix dates

prices_step1 <- raw_prices |>
  mutate(
    # first we check if it is YMD
    is_ymd = str_detect(date, "^[0-9]{4}[./-]"),
    # we assume the other cases are DMY
    
    clean_date = case_when(
      is_ymd ~ ymd(date),
      !is_ymd ~ dmy(date),
      TRUE ~ NA_Date_
    )
  )

raw_prices |>
  mutate(date_pattern = str_replace_all(date, "[0-9]", "9")) |>
  count(source_file, date_pattern) |>
  print(n = Inf)

sum(is.na(prices_step1$clean_date))
class(prices_step1$clean_date)
summary(prices_step1$clean_date)

#We get a warning that observations failed to parse. We try to check which
prices_step1$date[is.na(prices_step1$clean_date)]
#More warnings than NAs because case_when runs ymd() and dmy() on the whole column, not only on matching rows
unparsed_dates <- prices_step1 |> filter(!is.na(date), is.na(clean_date))


prices_step1 |>
  filter(!is.na(clean_date)) |>
  filter(str_extract(source_file, "[0-9]{4}_Q[0-9]") !=
           paste0(year(clean_date), "_Q", quarter(clean_date)))
#0 rows - every parsed date lies in the quarter of its file, so the DMY assumption holds


#First attempt: as.numeric(raw_prices$price) -> many NAs, because prices contain
#currency codes, spaces as thousands separator and decimal commas
#In prices the comma is DECIMAL (2 digits after it), in volumes it is THOUSANDS (3 digits after it)
prices_step2 <- prices_step1 |>
  mutate(
    price_currency = str_extract(price, "[A-Z]{3}"),                 # currency written in the price text
    price_text     = str_remove_all(price, "[A-Z]{3}|\\s"),          # remove currency and spaces
    price_text     = str_replace(price_text, ",", "."),              # decimal comma -> point
    clean_price    = suppressWarnings(as.numeric(price_text)),       # "not recorded" -> NA
    clean_volume   = suppressWarnings(as.numeric(str_remove_all(volume, "[ ,]"))),  # remove thousands separators
    price_problem  = is.na(clean_price) | clean_price <= 0,
    volume_problem = is.na(clean_volume) | clean_volume < 0
  )

#Check: only the invalid values seen in the patterns should remain
prices_step2 |> filter(price_problem)  |> count(price)
prices_step2 |> filter(volume_problem) |> count(volume)
#Only negative/zero values, "not recorded", "unknown" and NA are flagged -> conversion works for all other formats
summary(prices_step2$clean_price)


#Ticker matching
#reusable: works for any vector of observed tickers and any official list
propose_ticker_matches <- function(observed, official) {
  cleaned <- observed |> toupper() |> str_remove_all("[^A-Z0-9]") |> str_replace_all("0", "O")
  distances <- adist(cleaned, official)          # rows = observed, columns = official
  min_dist  <- apply(distances, 1, min)
  tibble(
    observed_ticker = observed,
    cleaned_ticker  = cleaned,
    min_distance    = min_dist,
    n_candidates    = rowSums(distances == min_dist),
    candidates      = apply(distances, 1, function(d) paste(official[d == min(d)], collapse = ", ")),
    decision = case_when(
      min_distance == 0 ~ "accepted: format fix",
      n_candidates == 1 & min_distance == 1 ~ "proposed: one closest ticker",
      TRUE ~ "unresolved: ambiguous or too different"
    )
  )
}

unknown_tickers <- raw_prices |> distinct(ticker) |> anti_join(companies, by = "ticker") |> pull(ticker)
ticker_review   <- propose_ticker_matches(unknown_tickers, companies$ticker)
ticker_review |> print(n = Inf)

#Review evidence: price level, currency and dates of each ticker
ticker_profile <- prices_step2 |>
  group_by(ticker) |>
  summarise(
    n_rows       = n(),
    median_price = median(clean_price[!price_problem], na.rm = TRUE),
    currencies   = paste(sort(unique(na.omit(c(currency, price_currency)))), collapse = ", "),
    first_date   = min(clean_date, na.rm = TRUE),
    last_date    = max(clean_date, na.rm = TRUE)
  )

#one row per observed ticker and candidate, side by side
ticker_evidence <- ticker_review |>
  separate_rows(candidates, sep = ", ") |>
  left_join(ticker_profile, by = c("observed_ticker" = "ticker")) |>
  left_join(ticker_profile, by = c("candidates" = "ticker"), suffix = c("_obs", "_cand")) |>
  left_join(companies |> select(ticker, official_currency = currency), by = c("candidates" = "ticker")) |>
  select(observed_ticker, candidates, decision, n_rows_obs,
         median_price_obs, median_price_cand, currencies_obs, currencies_cand, official_currency)

ticker_evidence |> print(n = Inf, width = Inf)

#do the observed rows share dates with the candidate? (0 = it fills gaps -> same company likely)
ticker_evidence |>
  rowwise() |>
  mutate(shared_dates = sum(prices_step2$clean_date[prices_step2$ticker == observed_ticker] %in%
                              prices_step2$clean_date[prices_step2$ticker == candidates])) |>
  select(observed_ticker, candidates, shared_dates)

#Stronger evidence: observed price vs candidate's price on the nearest date
nearest_candidate_price <- function(cand_ticker, date) {
  cand <- prices_step2[prices_step2$ticker == cand_ticker & !prices_step2$price_problem &
                         !is.na(prices_step2$clean_date), ]
  if (nrow(cand) == 0 || is.na(date)) return(NA_real_)
  cand$clean_price[which.min(abs(cand$clean_date - date))]
}

nearest_check <- ticker_evidence |>
  select(observed_ticker, candidates) |>
  inner_join(prices_step2 |> filter(!price_problem) |>
               select(observed_ticker = ticker, clean_date, clean_price),
             by = "observed_ticker") |>
  rowwise() |>
  mutate(cand_price = nearest_candidate_price(candidates, clean_date),
         ratio      = round(clean_price / cand_price, 2)) |>
  ungroup()

nearest_check |> print(n = Inf)

#Decision rule: accept a candidate only if ALL observed prices are within 5 % of the candidate's
#price on the nearest date, and exactly one candidate satisfies this. Otherwise unresolved.
ticker_decisions <- nearest_check |>
  group_by(observed_ticker, candidates) |>
  summarise(n_obs_rows = n(),
            prices_match = all(abs(ratio - 1) <= 0.05),
            .groups = "drop") |>
  group_by(observed_ticker) |>
  mutate(n_matching = sum(prices_match)) |>
  ungroup()

accepted_ticker_changes <- ticker_decisions |>
  filter(prices_match, n_matching == 1) |>
  transmute(observed_ticker, official_ticker = candidates,
            reason = "only candidate with price within 5% on nearest date")

unresolved_tickers <- ticker_review |>
  filter(!observed_ticker %in% accepted_ticker_changes$observed_ticker)

accepted_ticker_changes
unresolved_tickers   # 0 rows - all typos resolved

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
write_csv(accepted_ticker_changes, "data/processed/ticker_changes_accepted.csv")
write_csv(unresolved_tickers,      "data/processed/ticker_changes_unresolved.csv")

#Apply corrections, keep the original ticker
prices_step3 <- prices_step2 |>
  left_join(accepted_ticker_changes |> select(observed_ticker, official_ticker),
            by = c("ticker" = "observed_ticker")) |>
  mutate(ticker_corrected = !is.na(official_ticker),
         clean_ticker     = coalesce(official_ticker, ticker)) |>
  select(-official_ticker)

#Checks: no rows added by the join, all tickers now official
nrow(prices_step3) == nrow(prices_step2)
prices_step3 |> anti_join(companies, by = c("clean_ticker" = "ticker")) |> count(clean_ticker)
sum(prices_step3$ticker_corrected)

#Currency: compare stated currency, currency in price text and official currency
prices_step4 <- prices_step3 |>
  left_join(companies |> select(ticker, official_currency = currency),
            by = c("clean_ticker" = "ticker")) |>
  mutate(
    row_id = row_number(),
    currency_case = case_when(
      !is.na(currency) & !is.na(price_currency) & currency != price_currency ~ "column and text disagree",
      coalesce(currency, price_currency) == official_currency                ~ "matches official",
      is.na(currency) & is.na(price_currency)                                ~ "missing",
      TRUE                                                                   ~ "differs from official"
    )
  )

nrow(prices_step4) == nrow(prices_step3)
prices_step4 |> count(currency_case)
prices_step4 |> filter(currency_case %in% c("differs from official", "column and text disagree")) |>
  count(clean_ticker, official_currency, currency, price_currency)

#Decide the 8 mismatching rows by price level: compare with the same ticker's
#nearest price among rows that are in the official currency
nearest_official_price <- function(tick, date) {
  ref <- prices_step4[prices_step4$clean_ticker == tick & prices_step4$currency_case == "matches official" &
                        !prices_step4$price_problem & !is.na(prices_step4$clean_date), ]
  if (nrow(ref) == 0 || is.na(date)) return(NA_real_)
  ref$clean_price[which.min(abs(ref$clean_date - date))]
}

currency_review <- prices_step4 |>
  filter(currency_case == "differs from official") |>
  rowwise() |>
  mutate(nearest_price = nearest_official_price(clean_ticker, clean_date),
         ratio = round(clean_price / nearest_price, 2)) |>
  ungroup() |>
  select(row_id, source_file, clean_ticker, clean_date, price, official_currency, currency, nearest_price, ratio)

currency_review

#All 8 ratios are 0.99-1.03 -> prices are in the official currency, only the label is wrong
prices_step5 <- prices_step4 |>
  left_join(currency_review |> select(row_id, ratio), by = "row_id") |>
  mutate(
    currency_ok = currency_case != "differs from official" | abs(ratio - 1) <= 0.05,
    clean_currency = if_else(currency_ok, official_currency, NA_character_),
    currency_flag = case_when(
      currency_case == "matches official" & !is.na(currency) ~ "as stated",
      currency_case == "matches official"                    ~ "taken from price text",
      currency_case == "missing"                             ~ "inferred from companies.csv",
      currency_ok                                            ~ "corrected: price level matches official currency",
      TRUE                                                   ~ "unresolved"
    )
  ) |>
  select(-ratio)

#Checks
nrow(prices_step5) == nrow(prices_step4)
prices_step5 |> count(currency_flag)
sum(is.na(prices_step5$clean_currency))   # 0 = every row has a usable currency

#8398 + 18 = 8416 rows matching official currency, 315 inferred, 8 corrected, 0 missing

#2.2 Duplicates
#First attempt: distinct() on all columns
nrow(prices_step5) - nrow(distinct(prices_step5 |> select(-row_id)))
#-> removes 10 rows = the same 10 extra rows as our 20 exact duplicates (10 pairs),
#   but distinct() cannot detect CONFLICTS: 7 ticker-date pairs with different price or volume
#   would stay in the data and the ticker-date key would not be unique

prices_step6 <- prices_step5 |>
  group_by(clean_ticker, clean_date) |>
  mutate(n_key      = n(),
         n_versions = n_distinct(clean_price, clean_volume, clean_currency)) |>
  ungroup() |>
  mutate(duplicate_status = case_when(
    is.na(clean_date) ~ "no valid date",
    n_key == 1        ~ "unique",
    n_versions == 1   ~ "exact duplicate",
    TRUE              ~ "conflict"
  ))

prices_step6 |> count(duplicate_status)

#all conflicting records go to review, none is chosen
duplicate_conflicts <- prices_step6 |>
  filter(duplicate_status == "conflict") |>
  arrange(clean_ticker, clean_date)
duplicate_conflicts |> select(source_file, clean_ticker, clean_date, price, volume, clean_price, clean_volume)

#keep one row of each exact duplicate, drop conflicts
prices_dedup <- prices_step6 |>
  filter(duplicate_status %in% c("unique", "exact duplicate")) |>
  distinct(clean_ticker, clean_date, .keep_all = TRUE)

#Check: unique ticker-date keys
prices_dedup |> count(clean_ticker, clean_date) |> filter(n > 1)   # must be 0 rows

