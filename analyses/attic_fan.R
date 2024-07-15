library(tidyverse)
library(fs)
library(thematic)
thematic::thematic_on(bg = '#222222', fg = 'gray', accent = 'gray', font = 'Helvetica')

source('./r/functions.R')
# Is it getting more humid in Denver in the summer? Look at days an nights.

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
      guess_max = 1e5,
      na = c("", "<NA>"),
      col_types = cls
    ),
    .progress = "Reading Hourly CSVs"
  )

hourly <- 
hourly_list |> 
  bind_rows() |> 
  janitor::clean_names() |>
  select(date, tmp, dew) |>
  mutate(date = with_tz(date, 'America/Denver')) |> 
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
  ) |>
  mutate(across(
    c(temp_c, dew_c),
    ~ parse_number(.) / 10
  )) |>
  mutate(rh = relative_humidity(
    t = temp_c,
    dp = dew_c
  )) |>
  mutate(
    year = year(date),
    month = month(date, label = T),
    tod = cut(hour(date),
      breaks = c(0, 6, 12, 18, 24),
      labels = c("night", "morning", "afternoon", "evening"),
      right = F
    )
  ) |> 
  mutate(across(ends_with('qc'),
\(qc) fct(qc) |> 
    fct_collapse(
      'Passed' = c('0','1','4','5','9'),
      'Suspect' = c('2','6','A','P'),
      'Erroneous' = c('3','7')
    ))) 

monthweek <- function(d,w) {
  ceiling((d-w)/7) + 1
}




hourly |> 
  filter(year == 2023) |> 
  filter(tmp != '+9999,9') |> 
  filter(temp_qc == 'Passed') |>
  # filter(floor_date(date, 'day') == ymd('2024-01-01')) |> 
  # ggplot(aes(x = date, y = temp_c)) +
  mutate(night = floor_date(date - hours(8), 'day')) |>
  mutate(daily_high = max(temp_c),
          daily_low = min(temp_c),
        .by = night) |> 
          filter(hms::as_hms(date) <= hms::hms(0,0,22),
                 hms::as_hms(date) >= hms::hms(0,0,21)) |> 
        # print(n=50) |> 
  slice_max(order_by = date, by = night) |> 
  distinct(night, daily_high, daily_low, date, ten_pm = temp_c) |> 
  mutate(attic_fan = 
  case_when(
    daily_high < 23 ~ 'not needed',
    daily_low >= 20 ~ 'too warm overnight',
    ten_pm >= 23 ~ 'too warm at 10pm',
    daily_low < 10 ~ 'run on low',
    .default = 'run on high'
  )) |> #count(attic_fan)
  mutate(wd = wday(night),
      mw = monthweek(day(night), wday(night)) |> factor() |> fct_rev()) |> 
  ggplot(aes(x = wd, y = mw, fill = attic_fan)) +
  geom_tile() +
  facet_wrap(~ month(night, label = T, abbr = F)) +
  scale_fill_manual(values = c(
'run on high' = pal('average_mean'),
'run on low' = scales::muted(pal('average_mean')),
'not needed' = '#444444',
'day too cold' = pal('record_low'),
'too cold overnight' = pal('actual_low'),
'too warm at 10pm' = '#ffff00',
'too warm overnight' = '#ff0000')) #pal('record_high')))
