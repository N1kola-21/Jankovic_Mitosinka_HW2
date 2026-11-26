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

#2.3 Suspicious observations
#invalid prices / volumes (flagged in 2.1)
prices_dedup |> count(price_problem, volume_problem)

#preliminary day-to-day changes, only rows with a valid price
prices_ordered <- prices_dedup |>
  filter(!price_problem) |>
  arrange(clean_ticker, clean_date) |>
  group_by(clean_ticker) |>
  mutate(prev_date  = lag(clean_date),  prev_price = lag(clean_price),
         next_date  = lead(clean_date), next_price = lead(clean_price),
         change     = clean_price / prev_price - 1) |>
  ungroup()

large_changes <- prices_ordered |>
  filter(abs(change) > 0.5) |>
  select(row_id, clean_ticker, prev_date, prev_price, clean_date, clean_price,
         next_date, next_price, change)

large_changes |> print(n = Inf, width = Inf)

#Decision rule: a change is a one-day spike if the NEXT price returns within 10 % of the PREVIOUS price
spikes <- large_changes |> filter(abs(next_price / prev_price - 1) <= 0.10)

suspicious_review <- large_changes |>
  mutate(decision = case_when(
    row_id %in% spikes$row_id ~ "exclude: one-day spike, next price returns to previous level",
    paste(clean_ticker, prev_date) %in% paste(spikes$clean_ticker, spikes$clean_date) ~
      "retain: correct price, large change only because previous day was a spike",
    TRUE ~ "retain: persistent level shift, plausible market move"
  ))

suspicious_review |> count(decision)   # expected 8 / 8 / 11

#Implement: mark spikes as excluded, keep everything else
prices_step7 <- prices_dedup |>
  mutate(spike_excluded = row_id %in% spikes$row_id,
         usable_price   = !price_problem & !spike_excluded)

#Check: after excluding spikes, only the 11 retained level shifts exceed 50 %
prices_step7 |>
  filter(usable_price) |>
  arrange(clean_ticker, clean_date) |>
  group_by(clean_ticker) |>
  mutate(change = clean_price / lag(clean_price) - 1) |>
  ungroup() |>
  filter(abs(change) > 0.5) |>
  nrow()   # expected 11

#Graph: QUAN price around the two excluded spikes (±10 days, log scale so neighbours are visible)
quan_all    <- prices_step7 |> filter(clean_ticker == "QUAN", !price_problem) |> arrange(clean_date)
quan_spikes <- quan_all |> filter(spike_excluded)

quan_windows <- quan_spikes |>
  transmute(spike_date = clean_date, window = paste("Spike on", clean_date)) |>
  cross_join(quan_all) |>
  filter(abs(as.numeric(clean_date - spike_date)) <= 10)

spike_plot <- ggplot(quan_windows, aes(clean_date, clean_price)) +
  geom_line(colour = "grey70", linetype = "dashed") +                                   # original
  geom_line(data = filter(quan_windows, usable_price), colour = "steelblue") +          # cleaned
  geom_point(data = filter(quan_windows, usable_price), colour = "steelblue", size = 1.5) +
  geom_point(data = filter(quan_windows, spike_excluded), shape = 21, size = 5, stroke = 1.2, colour = "red") +
  geom_text(data = filter(quan_windows, spike_excluded),
            aes(label = paste0(round(clean_price), " CZK\nexcluded")),
            vjust = 1.8, size = 3, colour = "red") +
  facet_wrap(~ window, scales = "free_x") +
  scale_y_log10() +
  labs(title = "QUAN: one-day price spikes and surrounding prices",
       subtitle = "Dashed = original series, solid = cleaned series; red = excluded observation",
       x = NULL, y = "Price (CZK, log scale)") +
  theme_minimal()

spike_plot
dir.create("figures", showWarnings = FALSE)
ggsave("figures/fig1_quan_spikes.png", spike_plot, width = 9, height = 4.5, dpi = 300)

prices_dedup |> count(price_problem, volume_problem)

#Part 3

#3.1 Join cleaned prices with company info and FX data; convert to CZK
class(benchmark)
# Parse benchmark columns (imported as character in Part 1)
benchmark_clean <- benchmark |>
  mutate(
    clean_date = ymd(date),
    price_usd  = as.numeric(price)
  ) |>
  arrange(clean_date)

# Check benchmark for missing values or duplicate dates
stopifnot(
  sum(is.na(benchmark_clean$clean_date)) == 0,
  sum(is.na(benchmark_clean$price_usd)) == 0,
  nrow(benchmark_clean) == n_distinct(benchmark_clean$clean_date)
)

# Check fx_daily for duplicate (date, currency) pairs before joining
fx_dups <- fx_daily |>
  group_by(date, currency) |>
  filter(n() > 1) |>
  arrange(currency, date)

fx_dups

#We found three duplicate entries

# Deduplicate fx_daily: keep one row for exact duplicates; drop if conflicting rates exist
fx_dedup <- fx_daily |>
  group_by(date, currency) |>
  mutate(n_rates = n_distinct(czk_per_unit)) |>
  ungroup() |>
  filter(n_rates == 1) |>
  distinct(date, currency, .keep_all = TRUE) |>
  select(date, currency, czk_per_unit)

head(fx_dedup)

# Check that (date, currency) is now unique
stopifnot(nrow(fx_dedup) == nrow(distinct(fx_dedup, date, currency)))

# join cleaned prices with ...

prices_3_1 <- prices_step7 |>
  left_join(
    companies |> select(ticker, company, sector),
    by = c("clean_ticker" = "ticker")
  ) |>
  left_join(
    fx_dedup,
    by = c("clean_date" = "date", "clean_currency" = "currency")
  ) |>
  mutate(
    price_czk = if_else(
      usable_price & !is.na(czk_per_unit),
      clean_price * czk_per_unit,
      NA_real_
    )
  )

# Join USd benchmark with FX data
benchmark_3_1 <- benchmark_clean |>
  left_join(
    fx_dedup,
    by = c("clean_date" = "date", "currency" = "currency")
  ) |>
  mutate(
    price_czk = if_else(!is.na(czk_per_unit), price_usd * czk_per_unit, NA_real_)
  )

# Check unmatched records and unintended row multiplication
stopifnot(
  nrow(prices_3_1) == nrow(prices_step7),
  nrow(benchmark_3_1) == nrow(benchmark_clean)
)

sum(is.na(prices_3_1$sector))                              
sum(prices_3_1$usable_price & is.na(prices_3_1$czk_per_unit))
sum(is.na(benchmark_3_1$czk_per_unit))

#3.2. 
#For missing exchange rates we use the chronologically last observation
#We flag all imputed values
all_trading_dates <- sort(unique(c(
  benchmark_clean$clean_date,
  na.omit(prices_step7$clean_date)
)))

fx_locf <- expand_grid(
  date     = all_trading_dates,
  currency = unique(fx_dedup$currency)
) |>
  left_join(fx_dedup, by = c("date", "currency")) |>
  arrange(currency, date) |>
  group_by(currency) |>
  mutate(
    fx_rate_locf = czk_per_unit
  ) |>
  fill(fx_rate_locf, .direction = "down") |>
  ungroup() |>
  mutate(
    fx_imputed = is.na(czk_per_unit) & !is.na(fx_rate_locf),
    fx_status  = case_when(
      !is.na(czk_per_unit) ~ "observed",
      fx_imputed           ~ "imputed",
      TRUE                 ~ "unavailable (no prior rate)"
    )
  ) |>
  select(date, currency, fx_rate_locf, fx_imputed, fx_status)

#Now we apply the imputed rates to company prices and recalculate CZK prices
prices_3_2 <- prices_3_1 |>
  left_join(
    fx_locf,
    by = c("clean_date" = "date", "clean_currency" = "currency")
  ) |>
  mutate(
    fx_rate_used = coalesce(czk_per_unit, fx_rate_locf),
    price_czk    = if_else(
      usable_price & !is.na(fx_rate_used),
      clean_price * fx_rate_used,
      NA_real_
    ),
    usable_czk   = !is.na(price_czk)
  )

#Now we join it with benchmark while retaining original dates

benchmark_3_2 <- benchmark_3_1 |>
  left_join(
    fx_locf,
    by = c("clean_date" = "date", "currency" = "currency")
  ) |>
  mutate(
    fx_rate_used = coalesce(czk_per_unit, fx_rate_locf),
    price_czk    = if_else(!is.na(fx_rate_used), price_usd * fx_rate_used, NA_real_),
    usable_czk   = !is.na(price_czk)
  )

#Verify that nrow didnt change
stopifnot(
  nrow(prices_3_2) == nrow(prices_3_1),
  nrow(benchmark_3_2) == nrow(benchmark_3_1)
)

prices_3_2 |> count(fx_status, fx_imputed, usable_czk)
benchmark_3_2 |> count(fx_status, fx_imputed, usable_czk)


#3.3

company_returns_calc <- prices_3_2 |>
  filter(usable_czk) |>
  arrange(clean_ticker, clean_date) |>
  group_by(clean_ticker) |>
  mutate(
    prev_usable_date  = lag(clean_date),
    daily_return_czk  = price_czk / lag(price_czk) - 1,
    calendar_days_gap = as.numeric(clean_date - prev_usable_date)
  ) |>
  ungroup() |>
  select(row_id, prev_usable_date, daily_return_czk, calendar_days_gap)

head(company_returns_calc)

prices_part3 <- prices_3_2 |>
  left_join(company_returns_calc, by = "row_id")

head(prices_part3)

benchmark_returns_calc <- benchmark_3_2 |>
  filter(usable_czk) |>
  arrange(clean_date) |>
  mutate(
    prev_usable_date  = lag(clean_date),
    daily_return_czk  = price_czk / lag(price_czk) - 1,
    calendar_days_gap = as.numeric(clean_date - prev_usable_date)
  ) |>
  select(clean_date, prev_usable_date, daily_return_czk, calendar_days_gap)

benchmark_part3 <- benchmark_3_2 |>
  left_join(benchmark_returns_calc, by = "clean_date")

head(benchmark_part3)

#Final checks for part 3
stopifnot(
  nrow(prices_part3) == nrow(prices_step7),
  nrow(benchmark_part3) == nrow(benchmark_clean),
  all(is.na(prices_part3$daily_return_czk[!prices_part3$usable_czk]))
)

summary(prices_part3$daily_return_czk)
summary(prices_part3$calendar_days_gap)
summary(benchmark_part3$daily_return_czk)
summary(benchmark_part3$calendar_days_gap)

