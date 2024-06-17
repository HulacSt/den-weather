library(tidyverse)
source('R/functions.R')

yr <- c(1995:2012, 2014:2023)
station_id <- 72565003017
url <-
  "https://www.ncei.noaa.gov/data/global-summary-of-the-day/access/"

dir.create('data/daily/raw', recursive = T)

tibble(yr) |> 
  transmute(url = str_glue("{url}{yr}/{station_id}.csv"),
         destfile = str_glue('data/daily/raw/{yr}.csv')) |> 
  pmap(download.file)

hourly_url <- "https://www.ncei.noaa.gov/data/global-hourly/access/"

dir.create('data/hourly/raw', recursive = T)

tibble(yr) |> 
  transmute(url = str_glue("{hourly_url}{yr}/{station_id}.csv"),
            destfile = str_glue('data/hourly/raw/{yr}.csv')) |> 
  pmap(download.file)


