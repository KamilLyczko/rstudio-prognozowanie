day_of_year_number <- function(date) {
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
  offset <- c(1, 5)
  week = (obs_num %/% 7) + 1
  day = (obs_num %% 7) + offset[2] - 1
  if(day > 7) {
    day <- day - 7
    week <- week + 1
  }
  return(c(week, day))
}


#funkcja zwracaj¹ca numer obserwacji dla podanego numeru tygodnia i dnia
#argument jako wektor dwóch liczb (numer tygodnia, numer dnia)
calc_obs_num <- function(v) { 
  offset <- c(1, 5)
  obs_num <- (v[1] - 1) * 7 + v[2] - offset[2] + 1
  return(obs_num)
}

#funkcja zwracaj¹ca czas obserwacji (wektor z numerem tygodnia i dnia)
#argumentem jest liczba zmiennoprzecinkowa bêd¹ca indeksem w obiekcie ts
calc_obs_week <- function(i) {
  week <- i %/% 1
  day <- round((i - week) * 7) + 1
  return(c(week, day))
}

#funkcja zwracaj¹ca datê obserwacji
#argument jako numer obserwacji
get_date_of_obs_n <- function(obs_num) {
  return(data$date[obs_num])
}

#funkcja zwracaj¹ca datê obserwacji
#argument jako wektor okreœlaj¹cy czas obserwacji (numer tygodnia, numer dnia)
get_date_of_obs_v <- function(v) {
  obs_num <- calc_obs_num(v)
  return(data$date[obs_num])
}

#funkcja zwracaj¹ca datê obserwacji
#argument jako liczba zmiennoprzecinkowa bêd¹ca indeksem w obiekcie ts
get_date_of_obs_ts <- function(i) {
  obs_num <- calc_obs_num(calc_obs_week(i))
  return(data$date[obs_num])
}

#funkcja zwracaj¹ca liczbê zmiennoprzecinkow¹ bêd¹c¹ indeksem w obiekcie ts
#argument jako numer obserwacji
get_index_of_obs <- function(ts, obs_num) {
  return(time(ts)[obs_num])
}
 
#funkcja zwracaj¹ca prognozy metod naiwnych dla podanego szeregu i horyzontu
naive_forecasts <- function(ts, h) {
  naive_forecast <- naive(ts, h = h)
  snaive_forecast <- snaive(ts, h = h)
  drift_forecast <- rwf(ts, h = h, drift = TRUE)
  
  forecasts <- list(naive = naive_forecast, snaive = snaive_forecast, drift = drift_forecast)
  return(forecasts)
}

#funkcja generuj¹ca wykres czasowy z szeregiem ucz¹cym i prognozami metod naiwnych
#u¿ywa funkcji autoplot(), umieszcza numery tygodni na osi x
generate_naive_forecasts_plot <- function(ts, forecasts, title = "", ylab = "") {
  autoplot(ts) +
    autolayer(forecasts$naive, series = "Prosta metoda naiwna", PI = FALSE) +
    autolayer(forecasts$snaive, series = "Sezonowa metoda naiwna", PI = FALSE) +
    autolayer(forecasts$drift, series = "Przyrostowa metoda naiwna", PI = FALSE) +
    labs(title = title, x = "numer tygodnia", y = ylab, color = "Prognozy:")
}

#funkcja generuj¹ca wykres czasowy z szeregiem testowym i prognozami metod naiwnych
#u¿ywa funkcji autoplot(), umieszcza numery tygodni na osi x
generate_naive_test_comparison_plot <- function(ts, forecasts, title = "", ylab = "") {
  autoplot(ts) +
    autolayer(forecasts$naive, series = "Prosta metoda naiwna", PI = FALSE) +
    autolayer(forecasts$snaive, series = "Sezonowa metoda naiwna", PI = FALSE) +
    autolayer(forecasts$drift, series = "Przyrostowa metoda naiwna", PI = FALSE) +
    labs(title = title, x = "numer tygodnia", y = ylab, color = "Prognozy:")
}

#funkcja generuj¹ca wykres czasowy z szeregiem ucz¹cym i prognozami metod naiwnych
#u¿ywa funkcji ggplot(), umieszcza daty na osi x
generate_naive_forecasts_plot2 <- function(ts, forecasts, title = "", ylab = "") {
  train_dates <- as.Date(c(seq(get_date_of_obs_ts(get_index_of_obs(ts, 1)),
                               get_date_of_obs_ts(get_index_of_obs(ts, length(ts))), 1)))
  test_dates <- window(data$date, start = (length(forecasts$naive$x) + 1), 
                       end = length(forecasts$naive$x) + 30)
  x_labs <- c(seq(train_dates[1], test_dates[length(test_dates)], 20))
  ggplot(data = data.frame(x = train_dates, y = ts), aes(x, y)) +
    geom_line() +
    geom_line(data = data.frame(x = test_dates, y = forecasts$naive$mean), 
              aes(x, y, colour = "Prosta metoda naiwna")) +
    geom_line(data = data.frame(x = test_dates, y = forecasts$snaive$mean), 
              aes(x, y, colour = "Sezonowa metoda naiwna")) +
    geom_line(data = data.frame(x = test_dates, y = forecasts$drift$mean), 
              aes(x, y, colour = "Przyrostowa metoda naiwna")) +
    scale_x_date(breaks = x_labs, labels = x_labs, date_labels = "%d-%m-%Y") +
    labs(title = title, x = "data", y = ylab, color = "Prognozy:") +
    theme(plot.title = element_text(size = 10), axis.text.x = element_markdown(angle = 45, hjust = 1))
}

#funkcja generuj¹ca wykres czasowy z szeregiem testowym i prognozami metod naiwnych
#u¿ywa funkcji ggplot(), umieszcza daty na osi x
generate_naive_test_comparison_plot2 <- function(ts, forecasts, title = "", ylab = "") {
  dates <- as.Date(c(seq(get_date_of_obs_ts(get_index_of_obs(ts, 1)),
                         get_date_of_obs_ts(get_index_of_obs(ts, length(ts))), 1)))
  x_labs <- c(seq(dates[1], dates[length(dates)], 5))
  ggplot(data = data.frame(x = dates, y = ts), aes(x, y)) +
    geom_line() +
    geom_line(data = data.frame(x = dates, y = forecasts$naive$mean), 
              aes(x, y, colour = "Prosta metoda naiwna")) +
    geom_line(data = data.frame(x = dates, y = forecasts$snaive$mean), 
              aes(x, y, colour = "Sezonowa metoda naiwna")) +
    geom_line(data = data.frame(x = dates, y = forecasts$drift$mean), 
              aes(x, y, colour = "Przyrostowa metoda naiwna")) +
    scale_x_date(breaks = x_labs, labels = x_labs, date_labels = "%d-%m-%Y") +
    labs(title = title, x = "data", y = ylab, color = "Prognozy:") +
    theme(plot.title = element_text(size = 10), axis.text.x = element_markdown(angle = 45, hjust = 1))
}

#funkcja obliczaj¹ca b³êdy ex post dla podanych prognoz i wartoœci testowych
calculate_ex_post_errors <- function(forecast, test_ts) {
  n <- length(test_ts)
  me <- 0
  mae <- 0
  mse <- 0
  mape <- 0
  for(i in 1:n) {
    e <- test_ts[i] - forecast$mean[i]
    me <- me + e
    mae <- mae + abs(e)
    mse <- mse + e*e
    mape <- mape + abs(e)/test_ts[i]
  }
  me <- me/n
  mae <- mae/n
  mse <- mse/n
  rmse <- sqrt(mse)
  mape <- mape/n*100
  return(list(ME = me, MAE = mae, MSE = mse, RMSE = rmse, MAPE = mape))
}

save_forecasts_to_csv <- function(forecast, file_name) {
  intervals_list <- list()
  for(i in 1:length(forecast$level)) {
    intervals_list[[i]] <- data.frame(
      forecast$lower[, i],
      forecast$upper[, i]
    )
    names <- c(
      paste("lower", forecast$level[i], "%", sep = ""),
      paste("upper", forecast$level[i], "%", sep = "")
      )
    colnames(intervals_list[[i]]) <- names
  }
  start_date <- as.Date(get_date_of_obs_ts(get_index_of_obs(forecast$mean, 1)))
  dates <- as.Date(c(start_date:(start_date+length(forecast$mean) - 1)))
  df <- data.frame(
    h = c(1:length(forecast$mean)),
    date = format(dates, "%d.%m.%Y"),
    prediction = forecast$mean
  )
  directory <- paste("data_sheets/", file_name, sep = "")
  write.csv2(cbind(df, intervals_list), directory, row.names = FALSE)
}

