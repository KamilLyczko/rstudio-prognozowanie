#funkcja zwracająca prognozy metod naiwnych dla podanego szeregu i horyzontu
naive_forecasts <- function(ts, h) {
  naive_forecast <- naive(ts, h = h)
  snaive_forecast <- snaive(ts, h = h)
  drift_forecast <- rwf(ts, h = h, drift = TRUE)
  
  forecasts <- list(naive = naive_forecast, snaive = snaive_forecast, drift = drift_forecast)
  return(forecasts)
}

#funkcja generująca wykres czasowy z szeregiem uczącym i prognozami metod naiwnych
#używa funkcji autoplot(), umieszcza numery tygodni na osi x
generate_naive_forecasts_plot <- function(ts, forecasts, title = "", ylab = "") {
  autoplot(ts) +
    autolayer(forecasts$naive, series = "Prosta metoda naiwna", PI = FALSE) +
    autolayer(forecasts$snaive, series = "Sezonowa metoda naiwna", PI = FALSE) +
    autolayer(forecasts$drift, series = "Przyrostowa metoda naiwna", PI = FALSE) +
    labs(title = title, x = "numer tygodnia", y = ylab, color = "Prognozy:")
}

#funkcja generująca wykres czasowy z szeregiem testowym i prognozami metod naiwnych
#używa funkcji autoplot(), umieszcza numery tygodni na osi x
generate_naive_test_comparison_plot <- function(ts, forecasts, title = "", ylab = "") {
  autoplot(ts) +
    autolayer(forecasts$naive, series = "Prosta metoda naiwna", PI = FALSE) +
    autolayer(forecasts$snaive, series = "Sezonowa metoda naiwna", PI = FALSE) +
    autolayer(forecasts$drift, series = "Przyrostowa metoda naiwna", PI = FALSE) +
    labs(title = title, x = "numer tygodnia", y = ylab, color = "Prognozy:")
}

#funkcja generująca wykres czasowy z szeregiem uczącym i prognozami metod naiwnych
#używa funkcji ggplot(), umieszcza daty na osi x
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

#funkcja generująca wykres czasowy z szeregiem testowym i prognozami metod naiwnych
#używa funkcji ggplot(), umieszcza daty na osi x
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