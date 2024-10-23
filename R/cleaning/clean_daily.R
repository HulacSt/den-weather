library(janitor)
library(fs)
library(readr)
library(fs)
library(janitor)
library(dplyr)
library(purrr)
library(lubridate)
library(tidyr)
library(forcats)

format_daily <- function(daily_raw) {
  daily_raw |> 
    mutate(year = year(DATE),
           Year = as.character(year),
           month = month(DATE, label = T),
           day = day(DATE),
           day_of_year = `year<-`(DATE, 2000),
           precip = if_else(PRCP > 99, NA_real_, PRCP),
           across(c(TEMP,MIN,MAX),\(x) na_if(x, 9999.9))) |>  # 3 precip values of 100, erroneous per the docs at https://www.ncei.noaa.gov/data/global-summary-of-the-day/doc/readme.pdf
    separate_wider_position(FRSHTT,
                            widths = c(
                              'fog' = 1,
                              'rain' = 1,
                              'snow' = 1,
                              'hail' = 1,
                              'thunder' = 1,
                              'tornado' = 1
                            )) |>
    mutate(across(fog:tornado, as.logical)) |>
    rename(actual_low = MIN,
           actual_high = MAX,
           actual_mean = TEMP) |> 
    arrange(day_of_year) |> 
    mutate(actual_precip_ytd = cumsum(precip))
}

daily <- 
  list.files('data/daily/raw', full.names = T) |> 
  map(~read_csv(., show_col_types = F) |> format_daily()) |> 
  bind_rows() |> 
  janitor::clean_names()

write_rds(daily, file = 'data/daily/daily.rds', compress = 'gz')
