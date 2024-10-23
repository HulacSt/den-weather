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

source('R/relative_humidity.R')

cls <-
  cols(
    STATION = col_double(),
    DATE = col_datetime(format = ""),
    SOURCE = col_character(),
    LATITUDE = col_double(),
    LONGITUDE = col_double(),
    ELEVATION = col_double(),
    NAME = col_character(),
    REPORT_TYPE = col_character(),
    CALL_SIGN = col_character(),
    QUALITY_CONTROL = col_character(),
    .default = col_character()
  )

hourly_list <-
  dir_ls("data/hourly/raw") |>
  map(
    ~ read_csv(.,
      show_col_types = F,
      progress = F,
      guess_max = 1e5,
      na = c("", "<NA>"),
      col_types = cls
    ),
    .progress = "Reading Hourly CSVs"
  )

hourly_raw <-
  hourly_list |>
  bind_rows() |>
  janitor::clean_names() |>
  select(date, tmp, dew) |>
  separate_wider_delim(
    cols = tmp,
    delim = ",",
    names = c("temp_c", "temp_qc"),
    cols_remove = F
  ) |>
  separate_wider_delim(
    cols = dew,
    delim = ",",
    names = c("dew_c", "dew_qc")
  )

hourly <- 
hourly_raw |>
  mutate(
    date = with_tz(date, "America/Denver"),
    across(
      c(temp_c, dew_c),
      \(t) parse_number(t) / 10
    ),
    rh = relative_humidity(
      t = temp_c,
      dp = dew_c
    ),
    year = year(date),
    month = month(date, label = T),
    tod = cut(hour(date),
      breaks = c(0, 6, 12, 18, 24),
      labels = c("night", "morning", "afternoon", "evening"),
      right = F
    ), across(
      ends_with("qc"),
      \(qc) fct(qc) |>
        fct_collapse(
          "Passed" = c("0", "1", "4", "5", "9"),
          "Suspect" = c("2", "6", "A", "P"),
          "Erroneous" = c("3", "7")
        )
    )
  ) |>
  mutate(temp_c = na_if(temp_c, tmp != '+9999,9' & temp_qc == 'Passed'))



write_rds(hourly, file = 'data/hourly/hourly.rds', compress = 'gz')
