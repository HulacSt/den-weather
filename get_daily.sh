#!/bin/bash
yr="$(date --date='4 days ago' +%Y)"
target="data/daily/raw/$yr.csv"
url="https://www.ncei.noaa.gov/data/global-summary-of-the-day/access/$yr/72565003017.csv"
curl -f $url > $target