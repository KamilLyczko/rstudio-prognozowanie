# I sposób - szereg czasowy bez modyfikacji
#różnicowanie szeregu, poszukiwanie wstępnych modeli
train <- window(deaths3, end = weekly_freq_day_number(length(deaths3) - 30))
diff_list <- make_stationary_ts(train)
main_title <- 10
axis_titles <- 10
plots <- list()
plots[[1]] <- generate_ts_time_plot(train, "Wykres czasowy liczb śmierci dla 3 fali", 
                                    "liczba śmierci") + set_titles_size(main_title, axis_titles)
plots[[2]] <- generate_ts_time_plot(diff_list$diff_ts, 
                                    paste0("Wykres czasowy szeregu liczb śmierci dla 3 fali po różnicowaniach (sezonowe: ",
                                           diff_list$diffs[1], ", pierwsze: ", diff_list$diffs[2], ")")) +
  set_titles_size(main_title, axis_titles)
plots[[3]] <- ggAcf(diff_list$diff_ts) +
  ggtitle("Wykres autokorelacji szeregu liczb śmierci dla 3 fali po różnicowaniach") +
  set_titles_size(main_title, axis_titles)
plots[[4]] <- ggPacf(diff_list$diff_ts) +
  ggtitle("Wykres autokorelacji częściowej szeregu liczb śmierci dla 3 fali po różnicowaniach") +
  set_titles_size(main_title, axis_titles)
grid.arrange(grobs = plots, ncol = 1)
#różnicowanie uzyskane przez make_stationary_ts: sezonowe - 1, pierwsze - 0
#Na podstawie PACF: ARIMA(1,0,0)(2,1,0)[7]

#wybieranie najlepszych modeli spośród kandydatów
orders_cand <- list(c(1,0,0,2,1,0), c(1,0,0,1,1,0), c(1,0,0,3,1,0), c(1,0,0,2,1,1), c(1,0,0,2,1,2),
                    c(2,0,0,2,1,0), c(0,0,1,2,1,0), c(1,0,1,2,1,0), c(1,0,1,3,1,0), c(1,0,1,2,1,1))

best_models <- find_best_arima_models(train, orders_cand)

cat("Najlepsze modele:\n")
#najlepszy model z listy (wg aicc):
summary(best_models$best_model_aicc)
#najlepszy model z listy (wg błędów treningowych): 
summary(best_models$best_model_training_errors)
#najlepszy model wybrany automatycznie:
summary(best_models$best_model_auto)

#poszukiwanie modelu dającego najlepsze prognozy
test <- window(deaths3, start = weekly_freq_day_number(length(deaths3) - 29))
best_model <- best_models[[find_best_forecast_model(best_models, test)]]
#Najlepszy model:
summary(best_model)
#Najlepszy model: ARIMA(1,0,3)(0,1,2)[7] 

#prognozowanie z wykorzystaniem najlepszego modelu
forecast <- forecast(best_model, h = 30)
plots <- list(
  generate_fit_plot(best_model,
                    paste0("Wykres dopasowania modelu ", 
                           get_arima_model_type(best_model),
                           " do szeregu liczb śmierci dla 3 fali"),
                    "liczba śmierci"),
  generate_forecast_plot(forecast,
                         "Wykres prognoz liczb śmierci dla 3 fali",
                         "liczba śmierci"),
  generate_test_comparison_plot(test, forecast,
                                "Porównanie prognoz liczb śmierci z wartościami rzeczywistymi",
                                "liczba śmierci")
)
grid.arrange(grobs = plots, ncol = 1)
show(calculate_ex_post_errors(forecast, test))

#Najlepszy model: ARIMA(1,0,3)(0,1,2)[7]
#Błędy testowe:
#            ME     MAE      MSE     RMSE     MAPE
#value 139.1342 157.744 46337.02 215.2603 40.52129

#--------------------------------------------------------------------------------------------------------
# II sposób - szereg czasowy po transformacji Boxa-Coxa
add1_train <- train + 1 #zwiększenie wartości obserwacji o 1 (eliminacja 0)
lambda <- BoxCox.lambda(add1_train)
boxcox_add1_train <- BoxCox(add1_train, lambda)
diff_list <- make_stationary_ts(boxcox_add1_train)
time_plots <- list()
cor_plots <- list()
time_plots[[1]] <- generate_ts_time_plot(add1_train, 
                                         "Wykres czasowy liczb śmierci zwiększonych o 1 dla 3 fali", 
                                         "liczba śmierci + 1") +
  set_titles_size(main_title, axis_titles)
time_plots[[2]] <- generate_ts_time_plot(boxcox_add1_train, 
                                         paste0("Wykres czasowy liczb śmierci dla 3 fali po transformacji Boxa-Coxa (lambda = ",
                                                signif(lambda, 3), ")"), 
                                         "BoxCox(liczba śmierci + 1)") +
  set_titles_size(main_title, axis_titles)
time_plots[[3]] <- generate_ts_time_plot(diff_list$diff_ts, 
                                         paste0("Wykres czasowy szeregu liczby śmierci dla 3 fali po ", 
                                                "transformacji Box-Cox i różnicowaniach (sezonowe: ",
                                                diff_list$diffs[1], ", pierwsze: ", diff_list$diffs[2], ")")) +
  set_titles_size(main_title, axis_titles)
cor_plots[[1]] <- ggAcf(diff_list$diff_ts) +
  ggtitle(paste0("Wykres autokorelacji transformowanego szeregu liczb śmierci dla 3 fali po różnicowaniach ",
                 "s: ", diff_list$diffs[1], ", p: ", diff_list$diffs[2], ")")) +
  set_titles_size(main_title, axis_titles)
cor_plots[[2]] <- ggPacf(diff_list$diff_ts) +
  ggtitle(paste0("Wykres autokorelacji częściowej transformowanego szeregu liczb śmierci dla 3 fali ", 
                 "po różnicowaniach (s: ", diff_list$diffs[1], ", p: ", diff_list$diffs[2], ")")) +
  set_titles_size(main_title, axis_titles)
grid.arrange(grobs = time_plots, ncol = 1)
grid.arrange(grobs = cor_plots, ncol = 1)

#różnicowanie uzyskane przez make_stationary_ts: sezonowe - 1, pierwsze - 0
#zostanie wykonana dodatkowa pierwsza różnica
new_diff_list <- list(
  diff_ts = diff(diff_list[[1]]),
  diffs = c(diff_list[[2]][1], diff_list[[2]][2] + 1)
)
generate_autocorrelation_plots(new_diff_list$diff_ts, 
                               paste0("Wykres czasowy przekształconego szeregu liczb śmierci dla 3 fali ",
                                      "po różnicowaniach (sezonowe: ",
                                      new_diff_list$diffs[1], ", pierwsze: ", new_diff_list$diffs[2], ")"))

#Na podstawie ACF: ARIMA(0,1,1)(0,1,1)[7], ARIMA(0,1,8)(0,1,1)[7]
orders_cand <- list(c(0,1,1,0,1,1), c(0,1,1,0,1,2), c(0,1,1,1,1,0), c(0,1,1,2,1,0), c(0,1,1,1,1,1),
                    c(0,1,2,0,1,1), c(1,1,1,0,1,1), c(1,1,1,1,1,1), c(0,1,8,0,1,1), c(0,1,8,1,1,1),
                    c(0,1,8,2,1,1), c(1,1,8,0,1,1), c(1,1,8,1,1,1))
best_models <- find_best_arima_models(add1_train, orders_cand, lambda)
cat("Najlepsze modele:\n")
#najlepszy model z listy (wg aicc):
summary(best_models$best_model_aicc)
#najlepszy model z listy (wg błędów treningowych): 
summary(best_models$best_model_training_errors)
#najlepszy model wybrany automatycznie:
summary(best_models$best_model_auto)

#poszukiwanie modelu dającego najlepsze prognozy
add1_test <- test + 1
best_model2 <- best_models[[find_best_forecast_model(best_models, add1_test)]]
summary(best_model2)
show(calculate_ex_post_errors(forecast(best_models[[1]], h = 30), add1_test))
show(calculate_ex_post_errors(forecast(best_models[[2]], h = 30), add1_test))
show(calculate_ex_post_errors(forecast(best_models[[3]], h = 30), add1_test))

#Najlepszy model: ARIMA(1,0,1)(0,1,2)[7] with drift

#prognozowanie z wykorzystaniem najlepszego modelu
forecast2 <- forecast(best_model2, h = 30)
plots <- list(
  generate_fit_plot(best_model2,
                    paste0("Wykres dopasowania modelu ", 
                           get_arima_model_type(best_model2),
                           " do zmodyfikowanego szeregu liczb śmierci dla 3 fali"),
                    "liczba śmierci + 1"),
  generate_forecast_plot(forecast2,
                         "Wykres prognoz liczb śmierci dla 3 fali",
                         "liczba śmierci + 1"),
  generate_test_comparison_plot(add1_test, forecast2,
                                "Porównanie prognoz liczb śmierci z wartościami rzeczywistymi",
                                "liczba śmierci + 1")
)
adjusted_forecast <- make_adjusted_forecast_object(forecast2, 1)
plots2 <- list(
  generate_forecast_plot2(adjusted_forecast,
                          "Wykres prognoz liczb śmierci dla 3 fali",
                          "liczba śmierci"),
  generate_test_comparison_plot(test, adjusted_forecast,
                                "Porównanie prognoz liczb śmierci z wartościami rzeczywistymi",
                                "liczba śmierci")
)
grid.arrange(grobs = plots, ncol = 1)
grid.arrange(grobs = plots2, ncol = 1)

show(calculate_ex_post_errors(adjusted_forecast, test))

#Najlepszy model dla szeregu z zwiększonymi wartościami: ARIMA(1,0,1)(0,1,2)[7] with drift 
#Błędy testowe dla skorygowanego szeregu i jego prognoz
#            ME      MAE      MSE     RMSE     MAPE
#value 120.0439 139.2901 33646.66 183.4303 42.09634

rm(train, diff_list, main_title, axis_titles, plots, plots2)
rm(new_diff_list)
rm(orders_cand, best_models, test, best_model, forecast)
rm(time_plots, cor_plots, best_model2, forecast2, adjusted_forecast)
rm(add1_train, add1_test, boxcox_add1_train, lambda)

#-----------------------------------------------------------------------------------

#Najlepszy model - I sposób: ARIMA(1,0,3)(0,1,2)[7]
#Błędy testowe:
#            ME     MAE      MSE     RMSE     MAPE
#value 139.1342 157.744 46337.02 215.2603 40.52129

#Najlepszy model dla szeregu z zwiększonymi wartościami - II sposób: ARIMA(1,0,1)(0,1,2)[7] with drift 
#Błędy testowe dla skorygowanego szeregu i jego prognoz
#            ME      MAE      MSE     RMSE     MAPE
#value 120.0439 139.2901 33646.66 183.4303 42.09634

#Najlepszy: model II sposobu