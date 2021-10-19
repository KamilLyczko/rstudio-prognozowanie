data <- read.csv("data_poland.csv", sep = ";")
covid_poland <- data.frame(
  date = as.Date(data$date, format = "%d.%m.%Y"), 
  confirmed = data$confirmed,
  deaths = data$deaths,
  tests = data$tests)
covid_poland_ts7 <- ts(
  cbind(covid_poland$confirmed,
        covid_poland$deaths,
        covid_poland$tests),
  names = c("confirmed", "deaths", "tests"),
  frequency = 7,
  start = c(1, 5)) #5 dzieñ tygodnia w R to czwartek, numeracja od niedzieli jako 1
covid_poland_ts365 <- ts(
  cbind(covid_poland$confirmed,
        covid_poland$deaths,
        covid_poland$tests),
  names = c("confirmed", "deaths", "tests"),
  frequency = 365.25,
  start = c(2020, day_of_year_number("2020-03-05")))

confirmed7 <- covid_poland_ts7[, "confirmed"]
deaths7 <- covid_poland_ts7[, "deaths"]
tests7 <- covid_poland_ts7[, "tests"]

confirmed365 <- covid_poland_ts365[, "confirmed"]
deaths365 <- covid_poland_ts365[, "deaths"]
tests365 <- covid_poland_ts365[, "tests"]

#szeregi czasowe dla I fali (od 05.03.2020 do 05.08.2020) z czêstotliwoœci¹ tygodniow¹ 
confirmed7_1 <- window(confirmed7, end = weekly_freq_day_number(154))
deaths7_1 <- window(deaths7, end = weekly_freq_day_number(154))
tests7_1 <- window(tests7, end = weekly_freq_day_number(154))
#szeregi czasowe dla II fali (od 05.03.2020 do 20.10.2020) z czêstotliwoœci¹ tygodniow¹ 
confirmed7_2 <- window(confirmed7, end = weekly_freq_day_number(230))
deaths7_2 <- window(deaths7, end = weekly_freq_day_number(230))
tests7_2 <- window(tests7, end = weekly_freq_day_number(230))
#szeregi czasowe dla III fali (od 05.03.2020 do 15.03.2021) z czêstotliwoœci¹ tygodniow¹ 
confirmed7_3 <- window(confirmed7, end = weekly_freq_day_number(376))
deaths7_3 <- window(deaths7, end = weekly_freq_day_number(376))
tests7_3 <- window(tests7, end = weekly_freq_day_number(376))
#szeregi czasowe dla IV fali (od 05.03.2020 do 30.09.2021) z czêstotliwoœci¹ tygodniow¹ 
confirmed7_4 <- confirmed7
deaths7_4 <- deaths7
tests7_4 <- tests7

#szeregi czasowe dla I fali (od 05.03.2020 do 05.08.2020) z czêstotliwoœci¹ roczn¹ 
confirmed365_1 <- window(confirmed365, end = c(2020, day_of_year_number("2020-08-05")))
deaths365_1 <- window(deaths365, end = c(2020, day_of_year_number("2020-08-05")))
tests365_1 <- window(covid_poland_ts365[, "tests"], end = c(2020, day_of_year_number("2020-08-05")))
#szeregi czasowe dla II fali (od 05.03.2020 do 20.10.2020) z czêstotliwoœci¹ roczn¹
confirmed365_2 <- window(confirmed365, end = c(2020, day_of_year_number("2020-10-20")))
deaths365_2 <- window(deaths365, end = c(2020, day_of_year_number("2020-10-20")))
tests365_2 <- window(tests365, end = c(2020, day_of_year_number("2020-10-20")))
#szeregi czasowe dla III fali (od 05.03.2020 do 15.03.2021) z czêstotliwoœci¹ roczn¹
confirmed365_3 <- window(confirmed365, end = c(2021, day_of_year_number("2021-03-15") + 1))
deaths365_3 <- window(deaths365, end = c(2021, day_of_year_number("2021-03-15") + 1))
tests365_3 <- window(tests365, end = c(2021, day_of_year_number("2021-03-15") + 1))
#szeregi czasowe dla IV fali (od 05.03.2020 do 30.09.2021) z czêstotliwoœci¹ roczn¹ 
confirmed365_4 <- confirmed365
deaths365_4 <- deaths365
tests365_4 <- tests365
