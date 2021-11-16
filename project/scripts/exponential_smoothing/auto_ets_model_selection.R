c_plots <- list()
c_non_zero_plots <- list()
d_plots <- list()
for(i in 1:length(c_time_series)) {
  c_train <- window(c_time_series[[i]], end = weekly_freq_day_number(length(c_time_series[[i]]) - 30))
  d_train <- window(d_time_series[[i]], end = weekly_freq_day_number(length(d_time_series[[i]]) - 30))
  c_test <- window(c_time_series[[i]], start = weekly_freq_day_number(length(c_time_series[[i]]) - 29))
  d_test <- window(d_time_series[[i]], start = weekly_freq_day_number(length(d_time_series[[i]]) - 29))
  c_model <- ets(c_train, model = "ZZZ")
  c_model_non_zero <- ets(window(c_train, start = weekly_freq_day_number(2)), model = "ZZZ")
  d_model <- ets(d_train, model = "ZZZ")
  c_forecast <- forecast(c_model, h = 30)
  c_forecast_non_zero <- forecast(c_model_non_zero, h = 30)
  d_forecast <- forecast(d_model, h = 30)
  c_plots[[1]] <- generate_fit_plot(c_forecast,
                                    paste0("Wykres dopasowania modelu ", c_forecast$method, 
                                           " do szeregu liczb zakażeń dla ", i, " fali"),
                                    "liczba zakażeń")
  c_plots[[2]] <- generate_forecast_plot(c_forecast,
                                    paste0("Wykres czasowy prognoz liczb zakażeń dla ", i, " fali"),
                                    "liczba zakażeń")
  c_plots[[3]] <- generate_test_comparison_plot(c_test, c_forecast,
                                                paste0("Porównanie prognoz liczby zakażeń z szeregiem testowym dla ", 
                                                       i, " fali"), "liczba zakażeń")
  c_non_zero_plots[[1]] <- generate_fit_plot(c_forecast_non_zero,
                                    paste0("Wykres dopasowania modelu ", c_forecast_non_zero$method, 
                                           " do szeregu liczb zakażeń bez wartości 0 dla ", i, " fali"),
                                    "liczba zakażeń")
  c_non_zero_plots[[2]] <- generate_forecast_plot(c_forecast_non_zero,
                                         paste0("Wykres czasowy prognoz liczb zakażeń dla ", i, " fali"),
                                         "liczba zakażeń")
  c_non_zero_plots[[3]] <- generate_test_comparison_plot(c_test, c_forecast_non_zero,
                                                paste0("Porównanie prognoz liczby zakażeń z szeregiem testowym dla ", 
                                                       i, " fali"), "liczba zakażeń")
  d_plots[[1]] <- generate_fit_plot(d_forecast,
                                    paste0("Wykres dopasowania modelu ", d_forecast$method, 
                                           " do szeregu liczb śmierci dla ", i, " fali"),
                                    "liczba śmierci")
  d_plots[[2]] <- generate_forecast_plot(d_forecast,
                                         paste0("Wykres czasowy prognoz liczb śmierci dla ", i, " fali"),
                                         "liczba śmierci")
  d_plots[[3]] <- generate_test_comparison_plot(d_test, d_forecast,
                                                paste0("Porównanie prognoz liczby śmierci z szeregiem testowym dla ", 
                                                       i, " fali"), "liczba śmierci")
  grid.arrange(grobs = c_plots, ncol = 1)
  grid.arrange(grobs = c_non_zero_plots, ncol = 1)
  grid.arrange(grobs = d_plots, ncol = 1)
  
  cat("Model dla szeregu liczby zakażeń dla ", i, "fali:\n")
  show(summary(c_model))
  cat("\nBłędy treningowe:\n")
  show(calculate_training_errors(c_model))
  cat("\nBłędy testowe:\n")
  show(calculate_ex_post_errors(c_forecast, c_test))
  cat("\n----------------------------------------------\n")
  cat("Model dla szeregu liczby zakażeń dla ", i, "fali bez wartości 0:\n")
  show(summary(c_model_non_zero))
  cat("\nBłędy treningowe:\n")
  show(calculate_training_errors(c_model_non_zero))
  cat("\nBłędy testowe:\n")
  show(calculate_ex_post_errors(c_forecast_non_zero, c_test))
  cat("\n----------------------------------------------\n")
  cat("Model dla szeregu liczby śmierci dla ", i, "fali:\n")
  show(summary(d_model))
  cat("\nBłędy treningowe:\n")
  show(calculate_training_errors(d_model))
  cat("\nBłędy testowe:\n")
  show(calculate_ex_post_errors(d_forecast, d_test))
  cat("\n----------------------------------------------\n")
}

rm(i, c_train, d_train, c_test, d_test)
rm(c_model, c_model_non_zero, d_model)
rm(c_forecast, c_forecast_non_zero, d_forecast)
rm(c_plots, d_plots, c_non_zero_plots)