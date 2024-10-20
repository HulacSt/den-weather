#!/bin/bash
yr="$(date --date='4 days ago' +%Y)"
target="data/hourly/raw/$yr.csv"
url="https://www.ncei.noaa.gov/data/global-hourly/access/$yr/72565003017.csv"
curl -f $url > $target