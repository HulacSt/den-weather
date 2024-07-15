library(tidyverse)
library(here)
library(ggrepel)
# Load Data ####
# Normals and Records
if(file.exists('data/daily/daily_yrs.rds')) {
  daily_yrs <- read_rds('data/daily/daily_yrs.rds')
}

scale_x_months <- function() {
  scale_x_date(
    date_breaks = '1 month',
    date_minor_breaks = '1 month',
    date_labels = '%b',
    name = NULL
  )
}

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

get_data <- function(yr = year(today())) {
  daily <- get_year_data(yr)
  if(!exists('normals_records')) {
    normals_records <- 
      here('den_weather','normals_records.rds') |> 
      read_rds()
  }
  daily |> 
    select(date = DATE,day_of_year, actual_high,actual_mean,actual_low,actual_precip_ytd) |> 
    full_join(normals_records, by = ('day_of_year')) |> 
    mutate(record = case_when(
      record_high_year_set == yr
      & record_high_also_previous_years ~ 'Tied Record High',
      record_high_year_set == yr ~ 'New Record High',
      record_low_year_set == yr
      & record_low_also_previous_years ~ 'Tied Record Low',
      record_low_year_set == yr~ 'New Record Low',
      near(actual_low,record_low,.01) ~ 'Tied Record Low',
      actual_low < record_low ~ "New Record Low",
      near(actual_high,record_high,.01) ~ 'Tied Record High',
      actual_high > record_high ~ "New Record High",
      .default = NA_character_
    ),
    record_temp = if_else(str_detect(record, 'High'), coalesce(actual_high, record_high), coalesce(actual_low, record_low)),
    record_type = str_extract(record, '^\\w+\\s\\w+')
    )
}

pal_df <- 
  tibble::tribble(
    ~measure,          ~color,
    "actual_precip_ytd",       "#669bbc",
    "average_precip_ytd",       "#003049",
    "maximum_precipitation",      "navyblue",
    "maximum_snowfall",         "black",
    "average_precip_eoy",       "#003049",
    "actual_high",       "#f4a261",
    "record_high",       "#e76f51",
    "average_high",       "#e9c46a",
    "record_coldest_maximum",       "#e9c46a",
    "actual_mean",       "#721cb8",
    "average_mean",       "#509724",
    "actual",       "#721cb8",
    "actual_low",       "#2a9d8f",
    "record_low",       "#1d3557",
    "average_low", "paleturquoise",
    "record_warmest_minimum", "paleturquoise"
  )



pal_nv <- pal_df$color |> 
  `names<-`(pal_df$measure)

pal <- function(nm) {
  if(!nm %in% names(pal_nv)) {
    stop('Not a valid name. Here they are:\n', str_flatten(names(pal_nv), '\n'))
  } else {
    unname(pal_nv[nm])
  }
}; pal('average_high')


# Functions and Chart Bases ####
## Precipitation #######

plot_precip_comparison <- function(daily) {
  # idea: incorporate geom_ribbon
  
  # Get summary numbers for annotations
  anno <- 
    daily |> 
    filter(!is.na(actual_precip_ytd),
           date < today()) |> 
    slice_max(day_of_year) |> 
    relocate(contains('precip')) |> 
    select(day_of_year, contains('precip_ytd')) |> 
    pivot_longer(-day_of_year) |> 
    bind_rows(daily |>
                slice_max(day_of_year) |>
                transmute(day_of_year,
                          name = 'average_precip_eoy',
                          value = average_precip_ytd)
    ) |> 
    separate_wider_delim(cols = name,
                         names = c('aggregate','measure','timeframe'),
                         delim = "_",
                         cols_remove = F) |> 
    mutate(aggregate = str_to_title(aggregate),
           measure = 'Precipitation',
           timeframe = str_to_upper(timeframe),
           text = str_glue('{aggregate} Through {format(day_of_year, "%m/%d")}: {round(value,2)}"')) |> 
    mutate(text = if_else(day_of_year == ymd('2000-12-31') & timeframe == 'YTD' & aggregate == 'Average', NA_character_, text)) |> 
    filter(!is.na(text))
  
  daily |> 
    select(day_of_year, contains('precip_ytd')) |> 
    pivot_longer(-day_of_year) |> 
    filter(!is.na(value)) |> 
    ggplot(aes(x = day_of_year, y = value, color = name)) +
    geom_line(show.legend = F) +
    theme_minimal() +
    scale_color_manual(values = pal_nv,
                       name = NULL) +
    scale_x_months() +
    scale_y_continuous(minor_breaks = NULL, name = 'Inches') +
    theme(legend.position = 'bottom') +
    geom_point(data = anno, show.legend = F) +
    ggrepel::geom_text_repel(data = anno,
                             aes(x = day_of_year,
                                 y = value,
                                 label = text),
                             show.legend = F,
                             direction = 'both') +
    ggtitle(str_glue("{unique(year(daily$date))} Precipitation"))
}; #plot_precip_comparison(daily)

## Average Temps ####
plot_average_temps <- function(daily) {
  daily |> 
    select(day_of_year, actual_mean, average_mean) |> 
    pivot_longer(-day_of_year) |> 
    filter(!is.na(value)) |> 
    mutate(name = fct_rev(name)) |> 
    ggplot(aes(x = day_of_year, y = value, color = name)) +
    geom_line() +
    theme_minimal() +
    scale_color_manual(values = pal_nv, labels = c('Normal', 'Actual')) +
    scale_x_months() +
    labs(y = 'Average Temperature (F)', color = 'Year', title = str_glue('Daily Average Temperatures for {unique(daily$Year)}'))
}; #plot_average_temps(daily)

average_temps_facts <- function(daily) {
  avg_comp_norm <- 
    daily |> 
    filter(!is.na(actual_mean),
           date < today()) |> 
    mutate(comp_norm = case_when(
      near(actual_mean,  average_mean, .5) ~ 'Equal to Norm',
      actual_mean > average_mean ~ 'Above Norm',
      actual_mean < average_mean ~ 'Below Norm',
      .default = 'oh no'
    ))
  
  # heating_day_table <- 
  avg_comp_norm |> 
    count(comp_norm) |> 
    pivot_wider(names_from = comp_norm, values_from = n) |> 
    mutate(`Heating Days` = `Above Norm` - `Below Norm`) |> 
    pivot_longer(everything())
  
  # difference_from_normal_chart <- 
  #   avg_comp_norm |> 
  #   mutate(difference_from_normal = selected_year - Norm) |> 
  #   ggplot(aes(x = day_of_year,
  #              y = difference_from_normal,
  #              fill = comp_norm,
  #              # alpha = abs(difference_from_normal)
  #   )) +
  #   geom_col() +
  #   theme_minimal() +
  #   scale_fill_manual(values = c('Above Norm' = 'red',
  #                                'Below Norm' = 'blue',
  #                                'Equal to Norm' = 'green'))
  
  # return(list(heating_day_table = heating_day_table,
  #             difference_from_normal_chart = difference_from_normal_chart))
  
}; #average_temps_facts(daily)

temps_last_2_weeks <- function(daily) {
  daily |> 
    filter(!is.na(average_mean),
           date < today()) |> 
    slice_max(day_of_year, n = 14) |> 
    transmute(Day = format(day_of_year, '%m/%d'),
              Normal = average_mean,
              Actual = actual_mean)
}; #temps_last_2_weeks(daily)

# Extreme Temps ####
extreme_temps <- function(daily) {
  daily |> 
    mutate(high_range_upper = average_high + max_sd,
           high_range_lower = average_high - max_sd,
           low_range_upper = average_low + min_sd,
           low_range_lower = average_low - min_sd) |> 
    select(day_of_year,
           record_high,
           actual_high,
           average_high,
           average_low,
           actual_low,
           record_low,
           record,
           record_type,
           record_temp,
           high_range_upper,
           high_range_lower,
           low_range_upper,
           low_range_lower
    ) |> 
    pivot_longer(-c(day_of_year, record_type, record_temp, record,
                    high_range_upper,
                    high_range_lower,
                    low_range_upper,
                    low_range_lower)) |> 
    mutate(name = fct_inorder(name)) |> 
    filter(!is.na(value)) |>
    ggplot(aes(x = day_of_year,
               y = value)) +
    geom_ribbon(aes(ymin = high_range_lower,
                    ymax = high_range_upper),
                fill = pal_nv['average_high'],
                alpha = .3) +
    geom_ribbon(aes(ymin = low_range_lower,
                    ymax = low_range_upper),
                fill = pal_nv['average_low'],
                alpha = .3) +
    geom_line(aes(color = name, group = name)) +
    theme_minimal() +
    geom_point(aes(y = record_temp, shape = record_type),
               color = 'black', group = 'records', size = 4,
               show.legend = F) +
    scale_shape_manual(values = c('Tied Record' = '+',
                                  'New Record' = '*')) +
    scale_x_months() +
    scale_y_continuous(breaks = seq(-40,120, by = 20)) +
    labs(x = NULL, y = 'Degrees (F)') +
    scale_color_manual(values = pal_nv,
                       name = "Measure") +
    theme(legend.position = 'bottom')
  
}; #extreme_temps(daily)

records_table <- function(daily) {
  records_set <- 
    daily |>
    filter(!is.na(record),
           !is.na(day_of_year)) |>
    filter(!(is.na(date) & day_of_year == ymd('2020-02-29')))
  
  records_set |>
    count(record)
}; #temp_records(get_data(2012))

relative_humidity <- function(t, dp) {
  exp((17.625 * dp)/(243.04 + dp)) / exp((17.625 * t)/(243.04 + t))
}; #relative_humidity(20,15)
