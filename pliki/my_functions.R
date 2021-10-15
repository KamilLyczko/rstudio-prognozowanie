function(date) {
  date <- as.Date(date)
  month_number <- as.numeric(format(date, "%m"))
  day_number <- as.numeric(format(date, "%d"))
  year_number <- as.numeric(format(date, "%y"))
  month_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30);
  day_in_year <- day_number
  for (i in 1:month_number) {
    if(i==month_number)
      break
    day_in_year <- day_in_year + month_days[i]
  }
  if (!(year_number %% 4) && month_number > 2)
    day_in_year <- day_in_year + 1
  return(day_in_year)
}

weekly_freq_day_number <- function(obs_num) {
  return(c((obs_num %/% 7) + 1, obs_num %% 7))
}


