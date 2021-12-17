#funkcja zwracająca z podanej listy model z najmniejszą wartością aicc
find_model_best_aicc <- function(fit_list) {
  aiccs <- c()
  for(i in 1:length(fit_list)) {
    aiccs[i] <- fit_list[[i]]$aicc
  }
  return(fit_list[[match(min(aiccs), aiccs)]])
}

#funkcja zwracająca szereg czasowy po przekształceniach do postaci stacjonarnej
#oraz liczby wykonanych różnic c(sezonowe, pierwsze)
make_stationary_ts <- function(ts) {
  n_seas_diffs <- nsdiffs(ts) 
  if(n_seas_diffs)
    ts <- diff(ts, lag = frequency(ts))
  n_first_diffs <- 0
  while(TRUE) {
    if(ndiffs(ts)) {
      ts <- diff(ts)
      n_first_diffs <- n_first_diffs + 1
    }
    else
      break
  }
  if(!n_seas_diffs) {
    n_seas_diffs <- nsdiffs(ts) 
    if(n_seas_diffs)
      ts <- diff(ts, lag = frequency(ts))
  }
  result <- list(diff_ts = ts, diffs = c(n_seas_diffs, n_first_diffs)) 
}

#funkcja zwracająca listę trzech najlepszych modeli arima dla podanego szeregu czasowego
#1 model: najmniejsza wartość aicc dla modeli o podanych rzędach
#2 model: najwięcej najmniejszych wartości mierników błędóW treningowych dla modeli o podanych rzędach
#3 model: model generowany przez funkcję auto.arima()
#argument orders_list: lista wektorów określających rząd modelu: 
#elementy wektora o indeksach 1:3 określają część niesezonową modelu
#elementy wektora o indeksach 4:6 określają część sezonową modelu
find_best_arima_models <- function(ts, orders_list, lambda = NULL) {
  fit_list <- list()
  for(i in 1:length(orders_list)) {
    if(length(orders_list[[i]]) < 4)
      fit_list[[i]] <- Arima(ts, order = orders_list[[i]], lambda = lambda)
    else 
      fit_list[[i]] <- Arima(ts, order = c(orders_list[[i]][1:3]),
                              seasonal = c(orders_list[[i]][4:6]),
                              lambda = lambda)
  }
  result <- list(
    best_model_aicc = find_model_best_aicc(fit_list),
    best_model_training_errors = fit_list[[find_best_fitting_model(fit_list)]],
    best_model_auto = auto.arima(ts, lambda = lambda)
  )
}

#funkcja zwracająca łańcuch tekstowy opisujący rodzaj modelu ARIMA w przekazanym obiekcie 
get_arima_model_type <- function(fit) {
  orders <- fit$arma
  non_seasonal <- c(orders[1], orders[6], orders[2])
  seasonal <- c(orders[3], orders[7], orders[4])
  model <- paste0("ARIMA(", non_seasonal[1], ",", 
                  non_seasonal[2], ",",
                  non_seasonal[3], ")")
  if(length(which(seasonal == 0)) != 3)
    model <- paste0(model, "(", seasonal[1], ",",
                    seasonal[2], ",", seasonal[3], 
                    ")[", orders[5], "]")
  if("drift" %in% names(fit$coef))
    model <- paste0(model, " ze stałą")
  return(model)
}

#funkcja zwracająca listę z szeregami czasowymi z obiektu forecast
#(prognoza punktowa, szereg treningowy, wartości dopasowane) z wartościami
#obserwacji skorygowanymi o n
make_adjusted_forecast_object <- function(forecast, n) {
  adjusted <- list(
    mean = forecast$mean - n,
    x = forecast$x - n,
    fitted = forecast$fitted - n
  )
  return(adjusted)
}


generate_autocorrelation_plots <- function(ts, title = "", ylab = "") {
  main_title <- 10
  axis_titles <- 10
  plots <- list(
    generate_ts_time_plot(ts, title, ylab) + set_titles_size(main_title, axis_titles),
    ggAcf(ts) + set_titles_size(main_title, axis_titles) + ggtitle(""),
    ggPacf(ts) + set_titles_size(main_title, axis_titles) + ggtitle("")
  )
  grid.arrange(grobs = plots, ncol = 1)
}



