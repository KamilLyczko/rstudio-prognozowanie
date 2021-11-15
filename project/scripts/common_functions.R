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


#funkcja zwracająca numer obserwacji dla podanego numeru tygodnia i dnia
#argument jako wektor dwóch liczb (numer tygodnia, numer dnia)
calc_obs_num <- function(v) { 
  offset <- c(1, 5)
  obs_num <- (v[1] - 1) * 7 + v[2] - offset[2] + 1
  return(obs_num)
}

#funkcja zwracająca czas obserwacji (wektor z numerem tygodnia i dnia)
#argumentem jest liczba zmiennoprzecinkowa będąca indeksem w obiekcie ts
calc_obs_week <- function(i) {
  week <- i %/% 1
  day <- round((i - week) * 7) + 1
  return(c(week, day))
}

#funkcja zwracająca datę obserwacji
#argument jako numer obserwacji
get_date_of_obs_n <- function(obs_num) {
  return(data$date[obs_num])
}

#funkcja zwracająca datę obserwacji
#argument jako wektor określający czas obserwacji (numer tygodnia, numer dnia)
get_date_of_obs_v <- function(v) {
  obs_num <- calc_obs_num(v)
  return(data$date[obs_num])
}

#funkcja zwracająca datę obserwacji
#argument jako liczba zmiennoprzecinkowa będąca indeksem w obiekcie ts
get_date_of_obs_ts <- function(i) {
  obs_num <- calc_obs_num(calc_obs_week(i))
  return(data$date[obs_num])
}

#funkcja zwracająca liczbę zmiennoprzecinkową będącą indeksem w obiekcie ts
#argument jako numer obserwacji
get_index_of_obs <- function(ts, obs_num) {
  return(time(ts)[obs_num])
}

#funkcja zwracająca numer obserwacji
#argumenty: obiekt ts oraz indeks (liczba zmiennoprzecinkowa określająca czas)
get_numb_of_obs_ts <- function(ts, index) {
  return(calc_obs_num(calc_obs_week(index)))
}
 

#funkcja obliczająca błędy ex post dla podanych prognoz i wartości testowych
calculate_ex_post_errors <- function(forecast, test_ts) {
  n <- length(test_ts)
  j <- 0
  errors <- data.frame(
    ME = 0, MAE = 0, MSE = 0, RMSE = 0, MAPE = 0, row.names = "value"
  )
  for(i in 1:n) {
    e <- test_ts[i] - forecast$mean[i]
    errors["ME"] <- errors["ME"] + e
    errors["MAE"] <- errors["MAE"] + abs(e)
    errors["MSE"] <- errors["MSE"] + e*e
    if(test_ts[i] != 0) {
      errors["MAPE"] <- errors["MAPE"] + abs(e)/test_ts[i]
      j <- j + 1
    }
  }
  errors["ME"] <- errors["ME"]/n
  errors["MAE"] <- errors["MAE"]/n
  errors["MSE"] <- errors["MSE"]/n
  errors["RMSE"] <- sqrt(errors["MSE"])
  errors["MAPE"] <- errors["MAPE"]/j*100
  return(errors)
}

#funkcja zapisująca prognozy do pliku csv o podanej nazwie w podanym podkatalogu
save_forecasts_to_csv <- function(forecast, file_name, subdir = "") {
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
  finish_date <- as.Date(get_date_of_obs_ts(get_index_of_obs(forecast$mean, length(forecast$mean))))
  dates <- as.Date(c(seq(start_date, finish_date, 1)))
  df <- data.frame(
    h = c(1:length(forecast$mean)),
    date = format(dates, "%d.%m.%Y"),
    prediction = forecast$mean
  )
  if(stringi::stri_length(subdir) > 0)
    subdir <- paste0(subdir, "/")
  directory <- paste("data_sheets/", subdir, file_name, sep = "")
  write.csv2(cbind(df, intervals_list), directory, row.names = FALSE)
}

#funkcja zapisująca podaną ramkę danych do pliku csv o podanej nazwie
save_df_to_csv <- function(df, file_name, subdir = "") {
  if(stringi::stri_length(subdir) > 0)
    subdir <- paste0(subdir, "/")
  directory <- paste("data_sheets/", subdir, file_name, sep = "")
  write.csv2(df, directory, row.names = FALSE)
}

#funkcja nadająca wartości obserwacjom bez wartości
replace_na_values <- function(ts){
  ts_ind <- time(ts)[is.na(ts)] 
  for(i in 1:length(ts_ind)){
    ind <- calc_obs_num(calc_obs_week(ts_ind[i]))
    if((ind - 7) > 0)
      ts[ind] <- ts[ind - 7]
    else
      ts[ind] <- ts[ind - 1]
  }
  return(ts)
}


generate_test_comparison_plot <- function(test_ts, forecast_object, title = "", ylab = "") {
  dates <- as.Date(c(seq(get_date_of_obs_ts(get_index_of_obs(test_ts, 1)),
                         get_date_of_obs_ts(get_index_of_obs(test_ts, length(test_ts))), 1)))
  x_labs <- c(seq(dates[1], dates[length(dates)], 5))
  if(x_labs[length(x_labs)] != dates[length(dates)])
    x_labs[length(x_labs) + 1] <- dates[length(dates)]
  ggplot(data = data.frame(x = dates, y = test_ts), aes(x, y, colour = "Wartości rzeczywiste")) +
    geom_line() +
    geom_line(data = data.frame(x = dates, y = forecast_object$mean), 
              aes(x, y, colour = "Prognozy")) +
    scale_x_date(breaks = x_labs, labels = x_labs, date_labels = "%d-%m-%Y") +
    labs(title = title, x = "data", y = ylab, color = "") +
    theme(axis.text.x = element_markdown(angle = 45, hjust = 1))
}

get_indexes_of_zero_values <- function(ts) {
  j <- 0
  indexes <- c()
  for(i in 1:length(ts)) {
    if(ts[i] == 0) {
      j <- j + 1
      indexes[j] <- i
    }
  }
  return(indexes)
}

#nie działa
generate_forecast_plot <- function(forecast, title = "", ylab = "") {
  dates1 <- as.Date(c(seq(get_date_of_obs_ts(get_index_of_obs(forecast$x, 1)),
                          get_date_of_obs_ts(get_index_of_obs(forecast$x, length(forecast$x))), 1)))
  dates2 <- as.Date(c(seq(get_date_of_obs_ts(get_index_of_obs(forecast$mean, 1)),
                          get_date_of_obs_ts(get_index_of_obs(forecast$mean, length(forecast$mean))), 1)))
  x_labs <- c(seq(dates1[1], dates2[length(dates2)], 20))
  if(x_labs[length(x_labs)] != dates2[length(dates2)])
    x_labs[length(x_labs) + 1] <- dates2[length(dates2)]
  ggplot(data = data.frame(x = dates1, y = forecast$x), aes(x, y, colour = "Szereg treningowy")) +
    geom_line() +
    geom_line(data = data.frame(x = dates2, y = forecast$mean), aes(x, y, colour = "Prognozy")) +
    geom_line(data = data.frame(x = dates2, y = forecast$lower[, 1]), aes(x, y, colour = "Lower")) +
    geom_line(data = data.frame(x = dates2, y = forecast$upper[, 1]), aes(x, y, colour = "Upper")) +
    geom_ribbon(data = data.frame(x = dates2, y_min = forecast$lower[, 1],
                                y_max = forecast$upper[, 1]), aes(x = x, ymin = y_min, ymax = y_max)) +
    scale_x_date(breaks = x_labs, labels = x_labs, date_labels = "%d-%m-%Y") +
    labs(title = title, x = "data", y = ylab, color = "") +
    theme(axis.text.x = element_markdown(angle = 45, hjust = 1))
}

calculate_training_errors <- function(fit) {
  train_errors <- residuals(fit, type = "response")
  n <- length(train_errors)
  j <- 0
  errors <- data.frame(
    ME = 0, MAE = 0, MSE = 0, RMSE = 0, MAPE = 0, row.names = "value"
  )
  for(i in 1:n) {
    errors["ME"] <- errors["ME"] + train_errors[i]
    errors["MAE"] <- errors["MAE"] + abs(train_errors[i])
    errors["MSE"] <- errors["MSE"] + train_errors[i]*train_errors[i]
    if(fit$x[i] != 0) {
      errors["MAPE"] <- errors["MAPE"] + abs(train_errors[i])/fit$x[i]
      j <- j + 1
    }
  }
  errors["ME"] <- errors["ME"]/n
  errors["MAE"] <- errors["MAE"]/n
  errors["MSE"] <- errors["MSE"]/n
  errors["RMSE"] <- sqrt(errors["MSE"])
  errors["MAPE"] <- errors["MAPE"]/j*100
  return(errors)
}

generate_fit_plot <- function(fit, title = "", ylab = "") {
  dates <- as.Date(c(seq(get_date_of_obs_ts(get_index_of_obs(fit$x, 1)),
                         get_date_of_obs_ts(get_index_of_obs(fit$x, length(fit$x))), 1)))
  x_labs <- c(seq(dates[1], dates[length(dates)], length(dates)%/%10))
  if(as.numeric(x_labs[length(x_labs)] - dates[length(dates)]) > 5)
    x_labs[length(x_labs) + 1] <- dates[length(dates)]
  ggplot(data = data.frame(x = dates, y = fit$x), aes(x, y, colour = "Wartości rzeczywiste")) +
    geom_line() +
    geom_line(data = data.frame(x = dates, y = fitted(fit)), 
              aes(x, y, colour = "Wartości dopasowane")) +
    scale_x_date(breaks = x_labs, labels = x_labs, date_labels = "%d-%m-%Y") +
    labs(title = title, x = "data", y = ylab, color = "") +
    theme(axis.text.x = element_markdown(angle = 45, hjust = 1))
}

