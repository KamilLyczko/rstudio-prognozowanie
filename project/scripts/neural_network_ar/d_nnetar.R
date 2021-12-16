models <- list()
for(i in 1:length(d_time_series)) {
  train <- window(d_time_series[[i]], 
                  end = weekly_freq_day_number(
                    length(d_time_series[[i]]) - 30))
  test <- window(d_time_series[[i]],
                 start = weekly_freq_day_number(
                   length(d_time_series[[i]]) - 29))
  models[[i]] <- nnetar(train)
  predictions <- forecast(models[[i]], h = 30)
  plots <- list(
    generate_fit_plot(models[[i]],
                      paste0("Wykres dopasowania modelu ", 
                             get_nnar_model_type(models[[i]]),
                             " do szeregu liczb śmierci dla ", i, 
                             " fali"),
                      "liczba śmierci") + set_titles_size(),
    generate_forecast_plot2(predictions,
                            paste0("Wykres prognoz liczb śmierci dla ", 
                                   i, " fali"),
                            "liczba śmierci") + set_titles_size(),
    generate_test_comparison_plot(test, predictions,
                                  "Porównanie prognoz liczb śmierci z wartościami rzeczywistymi",
                                  "liczba śmierci") + set_titles_size()
  )
  grid.arrange(grobs = plots, ncol = 1)
  save_forecasts_to_csv(predictions, 
                        paste0("d", i, "_nnetar_forecasts.csv"), 
                        "neural_network_ar")
  save_df_to_csv(calculate_ex_post_errors(predictions, test),
                 paste0("d", i, "_nnetar_errors.csv"),
                 "neural_network_ar")
}

rm(models, train, test, predictions, plots, i)
