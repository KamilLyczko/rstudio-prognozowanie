#poszukiwanie najlepszych modeli dla liczb śmierci
#różne szeregi treningowe: zmienny numer pierwszej obserwacji
#dopasowywanie kilku modeli do szeregu (różne kombinacje trend/season)
#wybór najlepszego modelu dla każdego szeregu treningowego na podstawie 
# mierników błędów treningowych (najlepsze dopasowanie do danych hostorycznych)
#ostateczna selekcja modelu (i długości szeregu treningowego) na podstawie
# mierników błędów testowych (najlepsze dopasowanie prognoz wygasłych)

begin_list <- list(
  begin1 = c(1, 20, 30, 40, 50, 60, 70, 80, 90, 100),
  begin2 = c(1, 20, 40, 60, 80, 100, 120, 140, 160, 180),
  begin3 = c(1, 20, 60, 100, 140, 180, 220, 260, 300, 340),
  begin4 = c(1, 20, 60, 120, 180, 240, 300, 360, 420, 500)
)
best_fitting_models <- list(
  models1 = list(),
  models2 = list(),
  models3 = list(),
  models4 = list()
)

#poszukiwanie najlepszych modeli dla różnych długości szeregu treningowego
for(i in 1:length(begin_list[[1]])) {
  d_time_series_short <- list(
    window(d_time_series[[1]], start = weekly_freq_day_number(begin_list[[1]][i])),
    window(d_time_series[[2]], start = weekly_freq_day_number(begin_list[[2]][i])),
    window(d_time_series[[3]], start = weekly_freq_day_number(begin_list[[3]][i])),
    window(d_time_series[[4]], start = weekly_freq_day_number(begin_list[[4]][i]))
  )
  offset <- c(
    length(d_time_series[[1]]) - length(d_time_series_short[[1]]),
    length(d_time_series[[2]]) - length(d_time_series_short[[2]]),
    length(d_time_series[[3]]) - length(d_time_series_short[[3]]),
    length(d_time_series[[4]]) - length(d_time_series_short[[4]])
  )
  for(j in 1:length(d_time_series_short)) {
    train <- window(d_time_series_short[[j]], 
                    end = weekly_freq_day_number(length(d_time_series_short[[j]]) - 
                                                                                  30 + offset[j]))
    fit_list <- list(
      model1 = tslm(train ~ trend),
      model2 = tslm(train ~ season),
      model3 = tslm(train ~ trend + season)
    )
    best_fitting_models[[j]][[i]] <- fit_list[[find_best_fitting_model(fit_list)]]
  }
}

#wypisanie w konsoli inf. o modelach z najlepszym doposowaniem do szeregu treningowego
for(i in 1:length(best_fitting_models)) {
  cat("\nNajlepsze modele dla szeregu czasowego ", i, " fali:\n")
  for(j in 1:length(best_fitting_models[[i]])) {
    cat(begin_list[[i]][[j]], "\t")
    cat("deaths = ", as.character(best_fitting_models[[i]][[j]]$terms)[3], "\n")
  }
  cat("\n---------------------------------------------------\n")
}

#poszukiwanie najlepszego modelu do prognozowania
best_forecast_models <- list()

for(i in 1:length(best_fitting_models)) {
  test <- window(d_time_series[[i]], start = weekly_freq_day_number(length(d_time_series[[i]]) - 29))
  best_model_index <- find_best_forecast_model(best_fitting_models[[i]], test)
  best_forecast_models[[i]] <- list(
    first_obs_num = begin_list[[i]][best_model_index],
    model = best_fitting_models[[i]][[best_model_index]]
  )
}

#wypisanie informacji o najlepszych modelach do prognozowania

for(i in 1:length(best_forecast_models)) {
  cat("\nNajlepszy model do prognozowania szeregu czasowego ", i, " fali:\n")
  cat(best_forecast_models[[i]]$first_obs_num, "\t")
  cat("deaths = ", as.character(best_forecast_models[[i]]$model$terms)[3], "\n")
}

#wygenerowanie prognoz i wykresów
plots <- list()
for(i in 1:length(best_forecast_models)) {
  test <- window(d_time_series[[i]], start = weekly_freq_day_number(length(d_time_series[[i]]) - 29))
  forecast <- forecast(best_forecast_models[[i]]$model, h = 30)
  plot1_title <- paste0("Wykres dopasowania modelu deaths = ",
                        as.character(best_forecast_models[[i]]$model$terms)[3],
                        " do szeregu liczb śmierci dla ", i, " fali")
  plot2_title <- paste0("Wykres prognoz liczb śmierci dla ", i, " fali")
  plot3_title <- paste0("Porównanie prognoz z rzeczywistymi liczbami śmierci dla ", i, " fali")
  plots[[1]] <- generate_fit_plot(best_forecast_models[[i]]$model, plot1_title, "liczba śmierci")
  plots[[2]] <- generate_forecast_plot(forecast, plot2_title, "liczba śmierci")
  plots[[3]] <- generate_test_comparison_plot(test, forecast, plot3_title, "liczba śmierci")
  grid.arrange(grobs = plots, ncol = 1)
  # save_forecasts_to_csv(forecast, paste0("d", i, "_regression_forecasts.csv"), "regression")
  # save_df_to_csv(calculate_ex_post_errors(forecast, test), paste0("d", i, "_regression_errors.csv"),
  #                "regression")
}



rm(begin_list, best_fitting_models, d_time_series_short, offset)
rm(train, test, fit_list, i, j)
rm(best_forecast_models, best_model_index)
rm(forecast, plots, plot1_title, plot2_title, plot3_title)