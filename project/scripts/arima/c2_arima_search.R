#różnicowanie szeregu, poszukiwanie wstępnych modeli
train <- window(confirmed2, end = weekly_freq_day_number(length(confirmed2) - 30))
generate_ts_time_plot(train, "Wykres czasowy liczby zakażeń dla 2 fali", "liczba zakażeń")
diff_list <- make_stationary_ts(train)
generate_ts_time_plot(diff_list$diff_ts, 
                      paste0("Wykres czasowy szeregu liczby zakażeń dla 2 fali po różnicowaniach (sezonowe: ",
                             diff_list$diffs[1], ", pierwsze: ", diff_list$diffs[2], ")"))
ggAcf(diff_list$diff_ts) +
  ggtitle("Wykres autokorelacji szeregu liczby zakażeń dla 2 fali po różnicowaniach")
ggPacf(diff_list$diff_ts) +
  ggtitle("Wykres autokorelacji częściowej szeregu liczby zakażeń dla 2 fali po różnicowaniach")
#Na podstawie ACF: ARIMA(0,2,4)(0,0,1)[7]
#Na podstawie PACF: ARIMA(6,2,0)
#Modele do zbadania: ARIMA(0,2,4)(0,0,1)[7], ARIMA(0,2,4)(0,0,2)[7], ARIMA(0,2,4)(0,0,3)[7], 
#ARIMA(0,2,4)(0,0,4)[7],  ARIMA(0,2,3)(0,0,1)[7], ARIMA(4,2,0)(0,0,1)[7],
#ARIMA(0,2,2)(0,0,1)[7], ARIMA(0,2,5)(0,0,1)[7]


#wybieranie najlepszych modeli spośród kandydatów
orders_cand <- list(c(0,2,4,0,0,1), c(0,2,4,0,0,2), c(0,2,4,0,0,3), c(0,2,4,0,0,4),
                    c(0,2,3,0,0,1), c(0,2,2,0,0,1), c(0,2,5,0,0,1), c(4,2,0,0,0,1))
                    
best_models <- find_best_arima_models(train, orders_cand)

#-----------------------------------------------------------------------------------------
#inne różnicowanie
diff_train <- diff(train, lag = 7)
diff_train <- diff(diff_train)
generate_ts_time_plot(diff_train, 
                      paste0("Wykres czasowy szeregu liczby zakażeń dla 2 fali po różnicowaniach (sezonowe: 1",
                             ", pierwsze: 1)"))
ggAcf(diff_train) +
  ggtitle("Wykres autokorelacji szeregu liczby zakażeń dla 2 fali po różnicowaniach")
ggPacf(diff_train) +
  ggtitle("Wykres autokorelacji częściowej szeregu liczby zakażeń dla 2 fali po różnicowaniach")
#Na podstawie ACF: ARIMA(0,1,1)(0,1,0)[7]
#Na podstawie PACF: ARIMA(8,1,0)(0,1,0)[7]
orders_cand2 <- list(c(0,1,1,0,1,0), c(0,1,2,0,1,0), c(0,1,3,0,1,0), c(1,1,0,0,1,0), c(1,1,1,0,1,0))
best_models2 <- find_best_arima_models(train, orders_cand2)
#----------------------------------------------------------------------------------------
cat("Najlepsze modele:\n")
cat("zestaw 1:\n")
#najlepszy model z listy (wg aicc):
summary(best_models$best_model_aicc)
#najlepszy model z listy (wg błędów treningowych): 
summary(best_models$best_model_training_errors)
#najlepszy model wybrany automatycznie:
summary(best_models$best_model_auto)
cat("\nzestaw 2:\n")
#najlepszy model z listy (wg aicc):
summary(best_models2$best_model_aicc)
#najlepszy model z listy (wg błędów treningowych): 
summary(best_models2$best_model_training_errors)

checkresiduals(best_models$best_model_aicc) 
checkresiduals(best_models$best_model_training_errors) 
checkresiduals(best_models$best_model_auto) 
checkresiduals(best_models2$best_model_aicc) 
checkresiduals(best_models2$best_model_training_errors) 

#poszukiwanie modelu dającego najlepsze prognozy
test <- window(confirmed2, start = weekly_freq_day_number(length(confirmed2) - 29))
models <- list(best_models$best_model_aicc, best_models$best_model_training_errors,
               best_models$best_model_auto, best_models2$best_model_aicc,
               best_models2$best_model_training_errors)
best_model <- models[[find_best_forecast_model(models, test)]]
#Najlepszy model:
summary(best_model)
 
#prognozowanie z wykorzystaniem najlepszego modelu
forecast <- forecast(best_model, h = 30)
show(calculate_ex_post_errors(forecast, test))
plots <- list(
  generate_fit_plot(best_model,
                    paste0("Wykres dopasowania modelu ", 
                           get_arima_model_type(best_model),
                           " do szeregu liczb zakażeń dla 2 fali"),
                    "liczba zakażeń"),
  generate_forecast_plot(forecast,
                         "Wykres prognoz liczb zakażeń dla 2 fali",
                         "liczba zakażeń"),
  generate_test_comparison_plot(test, forecast,
                                "Porównanie prognoz liczb zakażeń z wartościami rzeczywistymi",
                                "liczba zakażeń")
)
grid.arrange(grobs = plots, ncol = 1)

rm(train, diff_list, orders_cand, best_models, test, best_model, forecast, plots)
rm(diff_train, orders_cand2, best_models2, models)





