#różnicowanie szeregu, poszukiwanie wstępnych modeli
train <- window(confirmed3, end = weekly_freq_day_number(length(confirmed3) - 30))
diff_list <- make_stationary_ts(train)
main_title <- 10
axis_titles <- 10
plots <- list()
plots[[1]] <- generate_ts_time_plot(train, "Wykres czasowy liczb zakażeń dla 3 fali", 
                      "liczba zakażeń") + set_titles_size(main_title, axis_titles)
plots[[2]] <- generate_ts_time_plot(diff_list$diff_ts, 
                      paste0("Wykres czasowy szeregu liczb zakażeń dla 3 fali po różnicowaniach (sezonowe: ",
                             diff_list$diffs[1], ", pierwsze: ", diff_list$diffs[2], ")")) +
  set_titles_size(main_title, axis_titles)
plots[[3]] <- ggAcf(diff_list$diff_ts) +
  ggtitle("Wykres autokorelacji szeregu liczb zakażeń dla 3 fali po różnicowaniach") +
  set_titles_size(main_title, axis_titles)
plots[[4]] <- ggPacf(diff_list$diff_ts) +
  ggtitle("Wykres autokorelacji częściowej szeregu liczb zakażeń dla 3 fali po różnicowaniach") +
  set_titles_size(main_title, axis_titles)
grid.arrange(grobs = plots, ncol = 1)
#różnicowanie uzyskane przez make_stationary_ts: sezonowe - 1, pierwsze - 0
#zostanie wykonana dodatkowa pierwsza różnica
new_diff_list <- list(
  diff_ts = diff(diff_list[[1]]),
  diffs = c(diff_list[[2]][1], diff_list[[2]][2] + 1)
)
generate_autocorrelation_plots(new_diff_list$diff_ts, 
                               paste0("Wykres czasowy szeregu liczb zakażeń dla 3 fali ",
                                      "po różnicowaniach (sezonowe: ",
                                      new_diff_list$diffs[1], ", pierwsze: ", new_diff_list$diffs[2], ")"))
#Na podstawie ACF: ARIMA(0,1,6)(0,1,1), ARIMA(0,1,6)(0,1,2)
#Na podstawie PACF: ARIMA(13,1,0)(2,1,0), ARIMA(6,1,0)(2,1,0)

#wybieranie najlepszych modeli spośród kandydatów
orders_cand <- list(c(0,1,6,0,1,1), c(0,1,6,0,1,2), c(0,1,6,1,1,1), c(0,1,6,2,1,1),
                    c(1,1,6,0,1,1), c(1,1,6,1,1,1), c(13,1,0,2,1,0), c(6,1,0,2,1,0))

best_models <- find_best_arima_models(train, orders_cand)

cat("Najlepsze modele:\n")
cat("zestaw 1:\n")
#najlepszy model z listy (wg aicc):
summary(best_models$best_model_aicc)
#najlepszy model z listy (wg błędów treningowych): 
summary(best_models$best_model_training_errors)
#najlepszy model wybrany automatycznie:
summary(best_models$best_model_auto)

#poszukiwanie modelu dającego najlepsze prognozy
test <- window(confirmed3, start = weekly_freq_day_number(length(confirmed3) - 29))
best_model <- best_models[[find_best_forecast_model(best_models, test)]]
#Najlepszy model:
summary(best_model)
#Najlepszy model: ARIMA(4,0,5)(0,1,2)[7]

#prognozowanie z wykorzystaniem najlepszego modelu
forecast <- forecast(best_model, h = 30)
plots <- list(
  generate_fit_plot(best_model,
                    paste0("Wykres dopasowania modelu ", 
                           get_arima_model_type(best_model),
                           " do szeregu liczb zakażeń dla 3 fali"),
                    "liczba zakażeń"),
  generate_forecast_plot(forecast,
                         "Wykres prognoz liczb zakażeń dla 3 fali",
                         "liczba zakażeń"),
  generate_test_comparison_plot(test, forecast,
                                "Porównanie prognoz liczb zakażeń z wartościami rzeczywistymi",
                                "liczba zakażeń")
)
grid.arrange(grobs = plots, ncol = 1)
show(calculate_ex_post_errors(forecast, test))

rm(train, diff_list, main_title, axis_titles, plots)
rm(new_diff_list, orders_cand, best_models, test, best_model, forecast)

#Najlepszy model: ARIMA(4,0,5)(0,1,2)[7]
#Błędy testowe:
#            ME      MAE      MSE     RMSE     MAPE
#value 2139.581 5007.605 35938182 5994.846 24.50686