library(tidyverse)

#Part 1
#1.1 Import
#Import of quarterly files
price_files <- list.files(pattern = "^stock_prices_.*\\.csv$", full.names = TRUE)

raw_prices <- read_csv(price_files, id = "source_file", col_types = cols(.default = "c")) |>
  mutate(source_file = basename(source_file))

companies <- read_csv("companies.csv")
benchmark <-  read_csv("market_benchmark.csv")

#fx_daily <- read_csv("fx_daily.csv")
#This gives a warning about parsing issues, when opening it as a text file we see it uses semicolons instead of commas thus we need to use read_csv2

fx_daily <- read_csv2("fx_daily.csv")

#1.2 Investigation

table(raw_prices$source_file)
nrow(companies)
nrow(benchmark)
nrow(fx_daily)

sapply(raw_prices, function(x) sum(is.na(x)))

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

sum(is.na(prices_step1$clean_date))
class(prices_step1$clean_date)
prices_step1$clean_date

#We get a warning that observations failed to parse. We try to check which
prices_step1$date[is.na(prices_step1$clean_date)]
#When inspecting first off all we get way less observations then the warning showed secondly none of these are a real date so we dont use them

