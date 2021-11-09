data <- read.csv("data_sheets/data_poland.csv", sep = ";")

data <- data.frame(
  date = as.Date(data$date, format = "%d.%m.%Y"),
  confirmed = ts(
    data$confirmed,
    frequency = 7,
    start = c(1, 5) #5 dzień tygodnia w R to czwartek, numeracja od niedzieli jako 1
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

confirmed <- data$confirmed #szereg czasowy liczby zaka?e?
deaths <- data$deaths       #szereg czasowy liczby ?mierci
tests <- data$tests         #szereg czasowy liczby test?w

#szeregi czasowe dla I fali (od 05.03.2020 do 04.09.2020)
confirmed1 <- window(confirmed, end = weekly_freq_day_number(184))
deaths1 <- window(deaths, end = weekly_freq_day_number(184))
tests1 <- window(tests, end = weekly_freq_day_number(184))
#szeregi czasowe dla II fali (od 05.03.2020 do 19.11.2020)
confirmed2 <- window(confirmed, end = weekly_freq_day_number(260))
deaths2 <- window(deaths, end = weekly_freq_day_number(260))
tests2 <- window(tests, end = weekly_freq_day_number(260))
#szeregi czasowe dla III fali (od 05.03.2020 do 14.04.2021)
confirmed3 <- window(confirmed, end = weekly_freq_day_number(406))
deaths3 <- window(deaths, end = weekly_freq_day_number(406))
tests3 <- window(tests, end = weekly_freq_day_number(406))
#szeregi czasowe dla IV fali (od 05.03.2020 do 25.10.2021)
confirmed4 <- confirmed
deaths4 <- deaths
tests4 <- tests

c_time_series <- list(confirmed1, confirmed2, confirmed3, confirmed4)
d_time_series <- list(deaths1, deaths2, deaths3, deaths4)
t_time_series <- list(tests1, tests2, tests3, tests4)
