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
  frequency = 7)
covid_poland_ts365 <- ts(
  cbind(covid_poland$confirmed,
        covid_poland$deaths,
        covid_poland$tests),
  names = c("confirmed", "deaths", "tests"),
  frequency = 365.25,
  start = c(2020, day_of_year_number("2020-03-05")))

#szeregi czasowe dla I fali (od 05.03.2020 do 05.08.2020) z czêstotliwoœci¹ tygodniow¹ 
confirmed7_1 <- window(covid_poland_ts7[, "confirmed"], end = weekly_freq_day_number(154))
deaths7_1 <- window(covid_poland_ts7[, "deaths"], end = weekly_freq_day_number(154))
tests7_1 <- window(covid_poland_ts7[, "tests"], end = weekly_freq_day_number(154))
#szeregi czasowe dla II fali (od 05.03.2020 do 20.10.2020) z czêstotliwoœci¹ tygodniow¹ 
confirmed7_2 <- window(covid_poland_ts7[, "confirmed"], end = weekly_freq_day_number(230))
deaths7_2 <- window(covid_poland_ts7[, "deaths"], end = weekly_freq_day_number(230))
tests7_2 <- window(covid_poland_ts7[, "tests"], end = weekly_freq_day_number(230))
#szeregi czasowe dla III fali (od 05.03.2020 do 15.03.2021) z czêstotliwoœci¹ tygodniow¹ 
confirmed7_3 <- window(covid_poland_ts7[, "confirmed"], end = weekly_freq_day_number(376))
deaths7_3 <- window(covid_poland_ts7[, "deaths"], end = weekly_freq_day_number(376))
tests7_3 <- window(covid_poland_ts7[, "tests"], end = weekly_freq_day_number(376))
#szeregi czasowe dla IV fali (od 05.03.2020 do 30.09.2021) z czêstotliwoœci¹ tygodniow¹ 
confirmed7_4 <- covid_poland_ts7[, "confirmed"]
deaths7_4 <- covid_poland_ts7[, "deaths"]
tests7_4 <- covid_poland_ts7[, "tests"]

#szeregi czasowe dla I fali (od 05.03.2020 do 05.08.2020) z czêstotliwoœci¹ roczn¹ 
confirmed365_1 <- window(covid_poland_ts365[, "confirmed"], end = c(2020, day_of_year_number("2020-08-05")))
deaths365_1 <- window(covid_poland_ts365[, "deaths"], end = c(2020, day_of_year_number("2020-08-05")))
tests365_1 <- window(covid_poland_ts365[, "tests"], end = c(2020, day_of_year_number("2020-08-05")))
#szeregi czasowe dla II fali (od 05.03.2020 do 20.10.2020) z czêstotliwoœci¹ roczn¹
confirmed365_2 <- window(covid_poland_ts365[, "confirmed"], end = c(2020, day_of_year_number("2020-10-20")))
deaths365_2 <- window(covid_poland_ts365[, "deaths"], end = c(2020, day_of_year_number("2020-10-20")))
tests365_2 <- window(covid_poland_ts365[, "tests"], end = c(2020, day_of_year_number("2020-10-20")))
#szeregi czasowe dla III fali (od 05.03.2020 do 15.03.2021) z czêstotliwoœci¹ roczn¹
confirmed365_3 <- window(covid_poland_ts365[, "confirmed"], end = c(2021, day_of_year_number("2021-03-15") + 1))
deaths365_3 <- window(covid_poland_ts365[, "deaths"], end = c(2021, day_of_year_number("2021-03-15") + 1))
tests365_3 <- window(covid_poland_ts365[, "tests"], end = c(2021, day_of_year_number("2021-03-15") + 1))
#szeregi czasowe dla IV fali (od 05.03.2020 do 30.09.2021) z czêstotliwoœci¹ roczn¹ 
confirmed365_4 <- covid_poland_ts365[, "confirmed"]
deaths365_4 <- covid_poland_ts365[, "deaths"]
tests365_4 <- covid_poland_ts365[, "tests"]
