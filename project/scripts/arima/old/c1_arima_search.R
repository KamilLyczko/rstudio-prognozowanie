#różnicowanie szeregu, poszukiwanie wstępnych modeli
train <- window(confirmed1, end = weekly_freq_day_number(length(confirmed1) - 30))
generate_ts_time_plot(train, "Wykres czasowy liczby zakażeń dla 1 fali", "liczba zakażeń")
diff_list <- make_stationary_ts(train)
generate_ts_time_plot(diff_list$diff_ts, 
                      paste0("Wykres czasowy szeregu liczby zakażeń dla 1 fali po różnicowaniach (sezonowe: ",
                             diff_list$diffs[1], ", pierwsze: ", diff_list$diffs[2], ")"))
ggAcf(diff_list$diff_ts) +
  ggtitle("Wykres autokorelacji szeregu liczby zakażeń dla 1 fali po różnicowaniach")
ggPacf(diff_list$diff_ts) +
  ggtitle("Wykres autokorelacji częściowej szeregu liczby zakażeń dla 1 fali po różnicowaniach")
#Na podstawie ACF: ARIMA(0,1,1), ARIMA(0,1,11)??
#Na podstawie PACF: ARIMA(1,1,0), ARIMA(10,1,0)??
#Modele do zbadania: ARIMA(0,1,1), ARIMA(0,1,2), ARIMA(0,1,3), ARIMA(1,1,1), ARIMA(1,1,0), ARIMA(2,1,0),
#ARIMA(3,1,0), ARIMA(2,1,2), ARIMA(0,1,11), ARIMA(0,1,10), ARIMA(0,1,12), ARIMA(10,1,0), ARIMA(9, 1, 0),
#ARIMA(11,1,0)

#wybieranie najlepszych modeli spośród kandydatów
orders_cand <- list(c(0,1,1), c(0,1,2), c(0,1,3), c(1,1,1), c(1,1,0), c(2,1,0), c(3,1,0),
                    c(2,1,2), c(0,1,11), c(0,1,10), c(0,1,12), c(10,1,0), c(9,1,0), c(11,1,0))
best_models <- find_best_arima_models(train, orders_cand)

summary(best_models$best_model_aicc)
#najlepszy model z listy (wg aicc): ARIMA(10,1,0)
summary(best_models$best_model_training_errors)
#najlepszy model z listy (wg błędów treningowych): ARIMA(11,1,0)
summary(best_models$best_model_auto)
#najlepszy model wybrany automatycznie: ARIMA(0,1,1)(0,0,1)[7] with drift

checkresiduals(best_models$best_model_aicc) #reszty to biały szum
checkresiduals(best_models$best_model_training_errors) #reszty to biały szum
checkresiduals(best_models$best_model_auto) #reszty nie są białym szumem

#poszukiwanie modelu dającego najlepsze prognozy
test <- window(confirmed1, start = weekly_freq_day_number(length(confirmed1) - 29))
best_model <- best_models[[find_best_forecast_model(best_models, test)]]
summary(best_model)
#Najlepszy model: ARIMA(10, 1, 0)

#prognozowanie z wykorzystaniem najlepszego modelu
forecast <- forecast(best_model, h = 30)
show(calculate_ex_post_errors(forecast, test))
plots <- list(
  generate_fit_plot(best_model,
                    paste0("Wykres dopasowania modelu ", 
                           get_arima_model_type(best_model),
                           " do szeregu liczb zakażeń dla 1 fali"),
                    "liczba zakażeń"),
  generate_forecast_plot(forecast,
                         "Wykres prognoz liczb zakażeń dla 1 fali",
                         "liczba zakażeń"),
  generate_test_comparison_plot(test, forecast,
                                "Porównanie prognoz liczb zakażeń z wartościami rzeczywistymi",
                                "liczba zakażeń")
)
grid.arrange(grobs = plots, ncol = 1)

rm(train, diff_list, orders_cand, best_models, test, best_model, forecast, plots)





