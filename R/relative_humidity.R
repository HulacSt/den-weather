relative_humidity <- function(t, dp) {
  exp((17.625 * dp)/(243.04 + dp)) / exp((17.625 * t)/(243.04 + t))
}; #relative_humidity(20,15)