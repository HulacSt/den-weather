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
  janitor::clean_names()

hourly |> slice_sample(n=10) |> View()

hourly |> 
  summarise(across(wnd:last_col(), ~mean(!is.na(.)))) |> 
  pivot_longer(everything()) |> 
  arrange(desc(value))

hourly |> 
  summarise(across(everything(), n_distinct))

hourly |> 
  select(date,source,report_type, quality_control, wnd:slp) |> 
  arrange(desc(date))

hourly |>
  select(date, tmp, dew) |>
  separate_wider_delim(
    cols = tmp,
    delim = ",",
    names = c("temp_c", "temp_qc")
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
    ))) -> h2

h2 |> 
  filter(temp_qc == 'Passed',
dew_qc == 'Passed',
rh <= 100) |> 
  summarise(across(c(temp_c, dew_c, rh),
mean),
.by = c(year,month,tod)) |> 
  ggplot(aes(x = year, y = rh, color = tod)) +
  geom_line() +
  facet_wrap(.~month)
