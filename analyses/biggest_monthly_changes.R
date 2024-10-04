library(tidyverse)
source('R/functions.R')
daily <- 
  list.files('data/daily/raw', full.names = T) |> 
  map(~read_csv(., show_col_types = F) |> format_daily()) |> 
  bind_rows() |> 
  janitor::clean_names()

monthly_averages <- 
daily |> 
  mutate(date = floor_date(date, 'month')) |> 
filter(n() >= 28, .by = date) |> 
  summarise(
    across(
      c(actual_mean, actual_high, actual_low),
      ~mean(., na.rm = T),
      .names = 'avg_daily_{str_remove(.col,"actual_")}'),
    .by = date) |> 
  pivot_longer(starts_with('avg')) |> 
  mutate(year = year(date), month = month(date, label = T))


monthly_averages |> 
  ggplot(aes(x = year, y = value, color = name)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(.~month)

mam <- 
monthly_averages |> 
  lm(formula = value ~ name * month * year)

broom::augment(mam, monthly_averages)

firsts <- 
monthly_averages |> 
  bind_cols(mv = predict(mam, monthly_averages)) |> 
  slice_min(order_by = year, by = c(name,month))

lasts <- 
monthly_averages |> 
  bind_cols(mv = predict(mam, monthly_averages)) |> 
  slice_max(order_by = year, by = c(name,month))

firsts |> 
  full_join(lasts, join_by(name, month), suffix = c('_first','_last')) |> 
  select(-starts_with('date')) |> 
  mutate(years_change = year_last - year_first,
         model_change = mv_last - mv_first) |> 
  relocate(month,name) |> 
  arrange(month,name) |> 
  mutate(dpy = model_change / years_change) |> 
  arrange(desc(abs(dpy))) |> 
  # select(-starts_with('value')) |> 
  print(n=20)

# September's average highs have increased significantly