library(tidyverse)
library(fs)
# library(thematic)
# thematic::thematic_on(bg = '#222222', fg = 'gray', accent = 'gray', font = 'Helvetica')

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
    ))) |> 
  filter(temp_c < 100)

c90 <- (90-32)*5/9

o90 <- 
hourly |> 
  filter(temp_qc == 'Passed') |> 
  mutate(dh = floor_date(date, 'hour'),
          day = floor_date(date, 'day'),
        year = year(date),
        over_90 = temp_c >= c90) |> 
  mutate(day_over_90 = if_else(over_90, day, NA), .by = day) |>
  summarise(n=n(),
  hours_over_90 = sum(over_90),
  days_over_90 = n_distinct(day_over_90, na.rm = T),
.by = year)

o90 |> 
  pivot_longer(ends_with('90')) |> 
  ggplot(aes(x = year, y = value, color = name)) +
    geom_line()
  
  o90 |> 
    ggplot(aes(x = year, y= hours_over_90)) +
    geom_col() +
    geom_smooth(method = 'lm')
  
  o90 |> 
    ggplot(aes(x = year, y= days_over_90)) +
    geom_col() +
    geom_smooth(method = 'lm')


lim <- 
  hourly |> 
  filter(temp_qc == 'Passed') |> 
  pull(temp_c) |> 
  range() |> 
  c_to_f()

hourly |> 
  filter(temp_qc == 'Passed') |> 
  mutate(temp_f = round(c_to_f(temp_c))) |> 
  count(temp_f) |> 
  ggplot(aes(x = temp_f, y = n)) +
  geom_col() +
    scale_fill_viridis_c(limits = lim, oob = scales::censor)

hourly |>
  filter(temp_qc == "Passed") |>
  mutate(ym = floor_date(date, "month")) |>
  filter(n() > 15 * 24, .by = ym) |>
  count(
    year = year(date),
    Month = month(date, label = T),
    temp = floor(c_to_f(temp_c) / 10) * 10
  ) |>
  ggplot(aes(x = year, y = n, fill = temp)) +
  geom_col(position = position_fill()) +
  facet_wrap(. ~ Month) +
  scale_y_continuous(
    name = "Percent of Observations",
    labels = scales::percent
  ) +
  # scale_fill_fermenter()
  scale_fill_viridis_c(limits = c(floor(lim[1]/10)*10,
ceiling(lim[2]/10)*10),
     oob = scales::censor,
     guide = guide_coloursteps(even.steps = T),
      option = "H")
