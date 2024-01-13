# parse Denver normals and min/maxes
library(tidyverse)
library(rvest)
library(here)

here::i_am('R/scrape_daily_records.R')


download_month <- function(m) {
  month <- month(m, label = T, abbr = F) |>
    as.character()
  url <- str_glue("https://www.weather.gov/bou/Climate_Record_{month}")
  pg <- read_html(url)
  return(pg)
}


colnames <-
  c("Month",
    "Day",
    "Normal High",
    "Record High",
    "Record High Year Set",
    "Record Warmest Minimum",
    "Record Warmest Minimum Year Set",
    "b1",
    "Normal Low",
    "Record Low",
    "Record Low Year Set",
    "Record Coldest Maximum",
    "Record Coldest Maximum Year Set",
    "b2",
    "Maximum Precipitation",
    "Maximum Precipitation Year Set",
    "Maximum Snowfall",
    "Maximum Snowfall Year Set") |> 
  str_to_lower() |> 
  str_replace_all(' ','_')

format_month <- function(pg) {
  tbl <- pg |> 
    html_table(header = T, convert = F) |> 
    pluck(2) 
  return(tbl)
}

pages <- map(1:12, download_month)

records <- 
map(pages, quietly(format_month)) |> 
  map('result') |> 
  bind_rows(.id = 'm') |> 
  `colnames<-`(colnames) |> 
  select(month,
         day,
         record_high,
         record_high_year_set,
         record_low,
         record_low_year_set,
         record_warmest_minimum,
         record_coldest_maximum,
         maximum_precipitation,
         maximum_snowfall) |>
  mutate(record_high_also_previous_years =
           str_detect(record_high_year_set, '^'),
         record_low_also_previous_years =
           str_detect(record_low_year_set, '^')) |> 
  mutate(across(where(is.character), \(x) parse_number(x, na = c("","NA","T"))))

write_csv(records, file = here('data', 'records.csv'))

################################################################################
################################################################################
################################################################################

# docs for below: https://www.ncei.noaa.gov/data/normals-daily/1991-2020/doc/Normals_DLY_Documentation_1991-2020.pdf
normals <- read_csv("https://www.ncei.noaa.gov/data/normals-daily/1991-2020/access/USW00003017.csv") |> 
  select(month, day,
         average_precip_ytd = `YTD-PRCP-NORMAL`,
         average_mean       = `DLY-TAVG-NORMAL`,
         average_high       = `DLY-TMAX-NORMAL`,
         average_low        = `DLY-TMIN-NORMAL`,
         mean_sd            = `DLY-TAVG-STDDEV`,
         min_sd             = `DLY-TMIN-STDDEV`,
         max_sd             = `DLY-TMAX-STDDEV`) |> 
  mutate(across(where(is.double), ~na_if(., 9999)),
         across(where(is.double), ~na_if(., -9999)),
         across(c(month,day), parse_number)) |> 
  mutate(day_of_year = make_date(year = 2000, month = month, day = day)) |>
  relocate(day_of_year)

write_csv(normals, here('data','normals.csv'))

