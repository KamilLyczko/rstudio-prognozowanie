# for(i in 1:length(d_time_series)) {
#   cat("Numery obserwacji o wartości 0 w szeregu liczb śmierci dla ", i, " fali:\n")
#   cat(get_indexes_of_zero_values(d_time_series[[i]]), "\n\n")
# }

models <- list()
for(i in 1:length(d_time_series)) {
  train <- window(d_time_series[[i]], end = weekly_freq_day_number(length(d_time_series[[i]]) - 30))
  models[[i]] <- ets(train)
}

cat("Modele szeregóW czasowych liczb śmierci:")
for(i in 1:4) {
  cat(i, " fala:\t", models[[i]]$method, "\n")
}

h <- 30
plots <- list()
for(i in 1:length(models)) {
  test <- window(d_time_series[[i]], start = weekly_freq_day_number(length(d_time_series[[i]]) - 29),
                 end = weekly_freq_day_number(length(d_time_series[[i]]) - 30 + h))
  forecast <- forecast(models[[i]], h = h)
  plot1_title <- paste0("Wykres dopasowania modelu ",
                        models[[i]]$method,
                        " do szeregu liczb śmierci dla ", i, " fali")
  plot2_title <- paste0("Wykres prognoz liczb śmierci dla ", i, " fali")
  plot3_title <- paste0("Porównanie prognoz z rzeczywistymi liczbami śmierci dla ", i, " fali")
  plots[[1]] <- generate_fit_plot(models[[i]], plot1_title, "liczba śmierci")
  plots[[2]] <- generate_forecast_plot(forecast, plot2_title, "liczba śmierci")
  plots[[3]] <- generate_test_comparison_plot(test, forecast, plot3_title, "liczba śmierci")
  grid.arrange(grobs = plots, ncol = 1)
  save_forecasts_to_csv(forecast, 
                        paste0("d", i, "_exp_smth_forecasts_h", h, ".csv"), 
                        "exponential_smoothing/ets")
  save_df_to_csv(calculate_ex_post_errors(forecast, test),
                 paste0("d", i, "_exp_smth_errors_h", h, ".csv"),
                 "exponential_smoothing/ets")
}

rm(i, models, train, test, h, forecast)
rm(plots, plot1_title, plot2_title, plot3_title)