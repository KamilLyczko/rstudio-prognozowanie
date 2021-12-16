# for(i in 1:length(c_time_series)) {
#   cat("Numery obserwacji o wartości 0 w szeregu liczb zakażeń dla ", i, " fali:\n")
#   cat(get_indexes_of_zero_values(c_time_series[[i]]), "\n\n")
# }

#poszukiwanie modeli najlepiej opisujących szereg treningowy (wstępna selekcja)
models <- c("ANN", "ANA", "AAN", "AAA", "MNN", 
            "MNA", "MNM", "MAN", "MAA", "MAM")
best_fitting_models <- list()
best_auto_chosen_models_non_zero <- list()
best_auto_chosen_models_zero <- list()
for(i in 1:length(c_time_series)) {
  train_non_zero <- window(c_time_series[[i]], 
                           start = weekly_freq_day_number(2),
                           end = weekly_freq_day_number(
                             length(c_time_series[[i]]) - 30))
  train_zero <- window(c_time_series[[i]], 
                       end = weekly_freq_day_number(
                         length(c_time_series[[i]]) - 30))
  fitted_models <- list()
  for(j in 1:length(models)) {
    fitted_models[[j]] <- ets(train_non_zero, 
                              model = models[j])
  }
  best_fitting_models[[i]] <- 
    fitted_models[[find_best_fitting_model(fitted_models)]]
  best_auto_chosen_models_non_zero[[i]] <- ets(train_non_zero)
  best_auto_chosen_models_zero[[i]] <- ets(train_zero)
}

mes <- "Najlepsze modele dopasowania do szeregów treningowych liczb zakażeń:\n\n"
for(i in 1:length(best_fitting_models)){
  mes <- paste0(mes, i, " fala - model dla szeregu bez 0 wybrany na podstawie mierników:\t",
                best_fitting_models[[i]]$method, "\n")
  mes <- paste0(mes, i, " fala - model dla szeregu bez 0 wybrany przez funkcję ets():\t\t",
                best_auto_chosen_models_non_zero[[i]]$method, "\n")
  mes <- paste0(mes, i, " fala - model dla szeregu z 0 wybrany przez funkcję ets():\t\t",
                best_auto_chosen_models_zero[[i]]$method, "\n\n")
}
cat(mes)

#poszukiwanie modeli generujących najlepsze prognozy na horyzont 30 dni (ostateczna selekcja)
best_models <- list()
for(i in 1:4) {
  test <- window(c_time_series[[i]], 
                 start = weekly_freq_day_number(
                   length(c_time_series[[i]]) - 29))
  models_list <- list(
    best_fitting_models[[i]], 
    best_auto_chosen_models_non_zero[[i]], 
    best_auto_chosen_models_zero[[i]]
  )
  best_models[[i]] <- list(
    model_index = 
      find_best_forecast_model(models_list, test),
    model = 
      models_list[[find_best_forecast_model(models_list, test)]]
  )
}

# cat("Modele generujące najlepsze prognozy liczb zakażeń:\n")
# for(i in 1:length(best_models)) {
#   cat(i, " fala\t", best_models[[i]]$method, "\n")
#   show(summary(best_models[[i]]))
#   show(calculate_training_errors(best_models[[i]]))
# }

mes <- "Modele generujące najlepsze prognozy liczb zakażeń:\n\n"
for(i in 1:length(best_models)){
  series <- switch(best_models[[i]]$model_index,
                   " dla szeregu bez 0 wybrany na podstawie mierników",
                   " dla szeregu bez 0 wybrany przez funkcję ets()",
                   " dla szeregu z 0 wybrany przez funkcję ets()")
  mes <- paste0(mes, i, " fala:\t",
                best_models[[i]]$model$method, 
                series, "\n")
}
cat(mes)

#Najlepsze modele:
#1 fala: ETS(A,A,A), bez wartości 0 (od 2 obserwacji) lista modeli
#2 fala: ETS(A,A,A), bez wartości 0 (od 2 obserwacji) lista modeli
#3 fala: ETS(M,N,M), bez wartości 0 (od 2 obserwacji) lista modeli
#4 fala: ETS(M,Ad,M), bez wartości 0 (od 2 obserwacji) ets

#wygenerowanie prognoz na podstawie najlepszych modeli
h <- 30
plots <- list()
for(i in 1:length(best_models)) {
  test <- window(c_time_series[[i]], start = weekly_freq_day_number(length(c_time_series[[i]]) - 29),
                 end = weekly_freq_day_number(length(c_time_series[[i]]) - 30 + h))
  forecast <- forecast(best_models[[i]]$model, h = h)
  plot1_title <- paste0("Wykres dopasowania modelu ",
                        best_models[[i]]$model$method,
                        " do szeregu liczb zakażeń dla ", i, " fali")
  plot2_title <- paste0("Wykres prognoz liczb zakażeń dla ", i, " fali")
  plot3_title <- paste0("Porównanie prognoz z rzeczywistymi liczbami zakażeń dla ", i, " fali")
  plots[[1]] <- generate_fit_plot(best_models[[i]]$model, plot1_title, "liczba zakażeń")
  plots[[2]] <- generate_forecast_plot(forecast, plot2_title, "liczba zakażeń")
  plots[[3]] <- generate_test_comparison_plot(test, forecast, plot3_title, "liczba zakażeń")
  grid.arrange(grobs = plots, ncol = 1)
  save_forecasts_to_csv(forecast, 
                        paste0("c", i, "_ets_forecasts.csv"), 
                        "exponential_smoothing/ets")
  save_df_to_csv(calculate_ex_post_errors(forecast, test),
                 paste0("c", i, "_ets_errors.csv"),
                 "exponential_smoothing/ets")
}

rm(i, j)
rm(models, best_fitting_models, best_auto_chosen_models_non_zero, best_auto_chosen_models_zero)
rm(train_non_zero, train_zero, fitted_models, best_models, models_list)
rm(mes, series)
rm(h, plots, plot1_title, plot2_title, plot3_title, test, forecast)
