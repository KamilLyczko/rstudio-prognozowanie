data <- read.csv("data_poland.csv", sep = ";")

data <- data.frame(
  date = as.Date(data$date, format = "%d.%m.%Y"),
  confirmed = ts(
    data$confirmed,
    frequency = 7,
    start = c(1, 5) #5 dzieñ tygodnia w R to czwartek, numeracja od niedzieli jako 1
  ),
  deaths = ts(
    data$deaths,
    frequency = 7,
    start = c(1, 5)
  ),
  tests = ts(
    data$tests,
    frequency = 7,
    start = c(1, 5)
  )
)

confirmed <- data$confirmed #szereg czasowy liczby zaka¿eñ
deaths <- data$deaths       #szereg czasowy liczby œmierci
tests <- data$tests         #szereg czasowy liczby testów

#szeregi czasowe dla I fali (od 05.03.2020 do 05.08.2020)
confirmed1 <- window(confirmed, end = weekly_freq_day_number(154))
deaths1 <- window(deaths, end = weekly_freq_day_number(154))
tests1 <- window(tests, end = weekly_freq_day_number(154))
#szeregi czasowe dla II fali (od 05.03.2020 do 20.10.2020)
confirmed2 <- window(confirmed, end = weekly_freq_day_number(230))
deaths2 <- window(deaths, end = weekly_freq_day_number(230))
tests2 <- window(tests, end = weekly_freq_day_number(230))
#szeregi czasowe dla III fali (od 05.03.2020 do 15.03.2021)
confirmed3 <- window(confirmed, end = weekly_freq_day_number(376))
deaths3 <- window(deaths, end = weekly_freq_day_number(376))
tests3 <- window(tests, end = weekly_freq_day_number(376))
#szeregi czasowe dla IV fali (od 05.03.2020 do 30.09.2021)
confirmed4 <- confirmed
deaths4 <- deaths
tests4 <- tests

c_time_series <- list(confirmed1, confirmed2, confirmed3, confirmed4)
d_time_series <- list(deaths1, deaths2, deaths3, deaths4)
