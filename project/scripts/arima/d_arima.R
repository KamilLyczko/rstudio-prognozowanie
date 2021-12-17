#dopasowanie modeli
models <- list()
for(i in 1:length(d_time_series)) {
  train <- window(d_time_series[[i]],
                  end = weekly_freq_day_number(
                    length(d_time_series[[i]]) - 30)) + 1
  models[[i]] <- auto.arima(train, 
                            lambda = "auto",
                            stepwise = FALSE,
                            approximation = FALSE)
}

mes <- "Automatycznie dopasowane modele:\n\n"
for(i in 1:length(models)){
  mes <- paste0(mes, i, " fala:\t",
                get_arima_model_type(models[[i]]), "\n")
}
cat(mes)

#prognozowanie na podstawie dobranych modeli
for(i in 1:length(models)) {
  test <- window(d_time_series[[i]],
                 start = weekly_freq_day_number(
                   length(d_time_series[[i]]) - 29))
  predictions <- make_adjusted_forecast_object(
    forecast(models[[i]], h = length(test)), 1
  )
  plot1_title <- paste0("Wykres dopasowania modelu ",
                        get_arima_model_type(models[[i]]),
                        " do szeregu liczb śmierci dla ",
                        i, " fali")
  plot2_title <- paste0("Wykres prognoz liczb śmierci dla ",
                        i, " fali")
  plot3_title <- 
    paste0("Porównanie prognoz z rzeczywistymi liczbami liczb śmierci dla ",
           i, " fali")
  plots <- list(
    generate_fit_plot(models[[i]],
                      plot1_title,
                      "liczba śmierci"),
    generate_forecast_plot2(predictions,
                           plot2_title,
                           "liczba śmierci"),
    generate_test_comparison_plot(test, predictions,
                                  plot3_title, "liczba śmierci")
  )
  grid.arrange(grobs = plots, ncol = 1)
  save_forecasts_to_csv(predictions,
                        paste0("d", i, "_arima_forecasts.csv"),
                        "arima")
  save_df_to_csv(calculate_ex_post_errors(predictions, test),
                 paste0("d", i, "_arima_errors.csv"),
                 "arima")
}

rm(models, train, test, plots, predictions, i)
rm(mes, plot1_title, plot2_title, plot3_title)