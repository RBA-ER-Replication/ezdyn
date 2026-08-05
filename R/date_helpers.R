# End of quarter and month functions.
# Defined separately because rbatools has some bad dependencies.

end_of_quarter <- function (date, q_offset = 0) 
{
  date <- check_date(date)
  if (q_offset != 0) {
    date <- lubridate::`%m+%`(date, months(q_offset * 3))
  }
  return(lubridate::ceiling_date(date, unit = "quarters") - 
           1)
}


end_of_month <- function (date, m_offset = 0) {
  date <- check_date(date)
  if (m_offset != 0) {
    date <- lubridate::`%m+%`(date, months(m_offset))
  }
  return(lubridate::ceiling_date(date, unit = "months") - 1)
}

start_of_month <- function (date, m_offset = 0) {
  date <- check_date(date)
  if (m_offset != 0) {
    date <- lubridate::`%m+%`(date, months(m_offset))
  }
  return(lubridate::floor_date(date, unit = "months"))
}

check_date <- function (date) {
  if (!lubridate::is.timepoint(date)) {
    date <- suppressWarnings(lubridate::ymd(date))
    if (all(is.na(date))) 
      stop("Argument `date` must be formatted as a Date, POSIXt, POSIXct or POSIXlt or a string in format '%Y-%m-%d'.\n")
  }
  return(date)
}
