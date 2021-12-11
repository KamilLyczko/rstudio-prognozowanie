#poszukiwanie najlepszych modeli dla liczb zakażeń
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

#poszukiwanie najlepszych modeli dla różnych długości 
# szeregu treningowego
for(i in 1:length(begin_list[[1]])) {
  c_time_series_short <- list(
    window(c_time_series[[1]], 
           start = weekly_freq_day_number(begin_list[[1]][i])),
    window(c_time_series[[2]], 
           start = weekly_freq_day_number(begin_list[[2]][i])),
    window(c_time_series[[3]], 
           start = weekly_freq_day_number(begin_list[[3]][i])),
    window(c_time_series[[4]], 
           start = weekly_freq_day_number(begin_list[[4]][i]))
  )
  t_time_series_cleaned_short <- list(
    window(t_time_series_cleaned[[1]], 
           start = weekly_freq_day_number(begin_list[[1]][i])),
    window(t_time_series_cleaned[[2]], 
           start = weekly_freq_day_number(begin_list[[2]][i])),
    window(t_time_series_cleaned[[3]], 
           start = weekly_freq_day_number(begin_list[[3]][i])),
    window(t_time_series_cleaned[[4]], 
           start = weekly_freq_day_number(begin_list[[4]][i]))
  )
  offset <- c(
    length(c_time_series[[1]]) - 
      length(c_time_series_short[[1]]),
    length(c_time_series[[2]]) - 
      length(c_time_series_short[[2]]),
    length(c_time_series[[3]]) - 
      length(c_time_series_short[[3]]),
    length(c_time_series[[4]]) - 
      length(c_time_series_short[[4]])
  )
  for(j in 1:length(c_time_series_short)) {
    c_train <- window(c_time_series_short[[j]], 
                    end = weekly_freq_day_number(
                      length(c_time_series_short[[j]]) - 
                                                   30 + offset[j]))
    t_train <- window(t_time_series_cleaned_short[[j]], 
                      end = weekly_freq_day_number(
                        length(t_time_series_cleaned_short[[j]]) - 
                                                     30 + offset[j]))
    fit_list <- list(
      model1 = tslm(c_train ~ trend),
      model2 = tslm(c_train ~ season),
      model3 = tslm(c_train ~ t_train),
      model4 = tslm(c_train ~ trend + season),
      model5 = tslm(c_train ~ trend + t_train),
      model6 = tslm(c_train ~ season + t_train),
      model7 = tslm(c_train ~ trend + season + t_train)
    )
    best_fitting_models[[j]][[i]] <- 
      fit_list[[find_best_fitting_model(fit_list)]]
  }
}

#wypisanie w konsoli inf. o modelach z najlepszym dopasowaniem 
# do szeregu treningowego
for(i in 1:length(best_fitting_models)) {
  cat("\nNajlepsze modele dla szeregu czasowego ", i, " fali:\n")
  for(j in 1:length(best_fitting_models[[i]])) {
    cat(begin_list[[i]][[j]], "\t")
    cat("confirmed = ", 
        as.character(best_fitting_models[[i]][[j]]$terms)[3], "\n")
  }
  cat("\n---------------------------------------------------\n")
}

#prognozowanie na podstawie określonych modeli (utworzenie listy z obiektami forecast)
forecasts <- list()
for(i in 1:length(best_fitting_models)) {
  forecasts[[i]] <- list()
  for(j in 1:length(best_fitting_models[[i]])) {
    terms <- paste0(best_fitting_models[[i]][[j]]$terms, 
                    collapse = ' ')
    if(grepl("t_train", terms, fixed = TRUE)) {
      t_test <- window(t_time_series_cleaned[[i]], 
                       start = weekly_freq_day_number(
                         length(t_time_series_cleaned[[i]]) - 29))
      forecasts[[i]][[j]] <- forecast(best_fitting_models[[i]][[j]],
                                      newdata = 
                                        data.frame(t_train = t_test))
    }
    else {
      forecasts[[i]][[j]] <- forecast(
        best_fitting_models[[i]][[j]], h = 30)
    }
  }
}

#poszukiwanie najlepszego modelu do prognozowania
best_forecast_models <- list()

for(i in 1:length(forecasts)) {
  c_test <- window(c_time_series[[i]], 
                   start = weekly_freq_day_number(
                     length(c_time_series[[i]]) - 29))
  best_model_index <- find_best_forecast(forecasts[[i]], c_test)
  best_forecast_models[[i]] <- list(
    first_obs_num = begin_list[[i]][best_model_index],
    model = forecasts[[i]][[best_model_index]]$model
  )
}

#wypisanie informacji o najlepszych modelach do prognozowania

for(i in 1:length(best_forecast_models)) {
  cat("\nNajlepszy model do prognozowania szeregu czasowego ", 
      i, " fali:\n")
  cat(best_forecast_models[[i]]$first_obs_num, "\t")
  cat("confirmed = ", 
      as.character(best_forecast_models[[i]]$model$terms)[3], "\n")
}

#wygenerowanie prognoz i wykresów
plots <- list()
for(i in 1:length(best_forecast_models)) {
  c_test <- window(c_time_series[[i]], start = weekly_freq_day_number(length(c_time_series[[i]]) - 29))
  forecast <- NULL
  if(grepl("t_train", 
           paste0(best_forecast_models[[i]]$model$terms, collapse = ' '), 
           fixed = TRUE)) {
    t_test <- window(t_time_series_cleaned[[i]], 
                     start = weekly_freq_day_number(length(t_time_series_cleaned[[i]]) - 29))
    forecast <- forecast(best_forecast_models[[i]]$model,
                                    newdata = data.frame(t_train = t_test))
  }
  else {
    forecast <- forecast(best_forecast_models[[i]]$model, h = 30)
  }
  plot1_title <- paste0("Wykres dopasowania modelu confirmed = ",
                        as.character(best_forecast_models[[i]]$model$terms)[3],
                        " do szeregu liczb zakażeń dla ", i, " fali")
  plot2_title <- paste0("Wykres prognoz liczb zakażeń dla ", i, " fali")
  plot3_title <- paste0("Porównanie prognoz z rzeczywistymi liczbami zakażeń dla ", i, " fali")
  plots[[1]] <- generate_fit_plot(best_forecast_models[[i]]$model, plot1_title, "liczba zakażeń")
  plots[[2]] <- generate_forecast_plot(forecast, plot2_title, "liczba zakażeń")
  plots[[3]] <- generate_test_comparison_plot(c_test, forecast, plot3_title, "liczba zakażeń")
  grid.arrange(grobs = plots, ncol = 1)
  save_forecasts_to_csv(forecast, paste0("c", i, "_regression_forecasts.csv"), 
                        "regression/forecasts")
  save_df_to_csv(calculate_ex_post_errors(forecast, c_test), paste0("c", i, "_regression_errors.csv"),
                 "regression")
}

rm(begin_list, best_fitting_models, c_time_series_short, t_time_series_cleaned_short, offset)
rm(c_train, t_train, c_test, t_test, fit_list, i, j)
rm(best_forecast_models, best_model_index, terms, forecasts)
rm(forecast, plots, plot1_title, plot2_title, plot3_title)