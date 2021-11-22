#różnicowanie szeregu, poszukiwanie wstępnych modeli
train <- window(confirmed1, end = weekly_freq_day_number(length(confirmed1) - 30))
generate_ts_time_plot(train, "Wykres czasowy liczby zakażeń dla 1 fali", "liczba zakażeń")
n_seas_diffs <- nsdiffs(train) #liczba wymaganych różnic sezonowych
if(n_seas_diffs)
  train_diff <- diff(train, lag = n_seas_diffs) else
  train_diff <- train
n_first_diffs <- ndiffs(train_diff)
if(n_first_diffs)
  train_diff <- diff(train_diff, lag = n_first_diffs)
generate_ts_time_plot(train_diff, 
                      paste0("Wykres czasowy szeregu liczby zakażeń dla 1 fali po różnicowaniach (sezonowe: ",
                             n_seas_diffs, ", pierwsze: ", n_first_diffs, ")"))
ggtsdisplay(train_diff)
ggAcf(train_diff) +
  ggtitle("Wykres autokorelacji szeregu liczby zakażeń dla 1 fali po różnicowaniach")
ggPacf(train_diff) +
  ggtitle("Wykres autokorelacji częściowej szeregu liczby zakażeń dla 1 fali po różnicowaniach")
#Na podstawie ACF: ARIMA(0,1,1), ARIMA(0,1,11)??
#Na podstawie PACF: ARIMA(1,1,0), ARIMA(10,1,0)??
#Modele do zbadania: ARIMA(0,1,1), ARIMA(0,1,2), ARIMA(0,1,3), ARIMA(1,1,1), ARIMA(1,1,0), ARIMA(2,1,0),
#ARIMA(3,1,0), ARIMA(2,1,2), ARIMA(0,1,11), ARIMA(0,1,10), ARIMA(0,1,12), ARIMA(10,1,0), ARIMA(9, 1, 0),
#ARIMA(11,1,0)

#wybieranie najlepszych modeli spośród kandydatów
orders_cand <- list(c(0,1,1), c(0,1,2), c(0,1,3), c(1,1,1), c(1,1,0), c(2,1,0), c(3,1,0),
                    c(2,1,2), c(0,1,11), c(0,1,10), c(0,1,12), c(10,1,0), c(9,1,0), c(11,1,0))
fit_list <- list()
for(i in 1:length(orders_cand)) {
  fit_list[[i]] <- Arima(train, order = orders_cand[[i]])
}

best_model_aicc <- find_model_best_aicc(fit_list)
summary(best_model_aicc)
show(calculate_training_errors(best_model_aicc))
#najlepszy model z listy (wg aicc): ARIMA(10,1,0)

best_model_training_errors <- fit_list[[find_best_fitting_model(fit_list)]]
summary(best_model_training_errors)
show(calculate_training_errors(best_model_training_errors))
#najlepszy model z listy (wg błędów treningowych): ARIMA(11,1,0)

auto_search <- auto.arima(train)
summary(auto_search)
show(calculate_training_errors(auto_search))
#najlepszy model wybrany automatycznie: ARIMA(0,1,1)(1,0,0)[7]

model_list <- list(best_model_aicc, best_model_training_errors, auto_search)
checkresiduals(best_model_aicc) #reszty to biały szum
checkresiduals(best_model_training_errors) #reszty to biały szum
checkresiduals(auto_search) #reszty nie są białym szumem

#poszukiwanie modelu dającego najlepsze prognozy
test <- window(confirmed1, start = weekly_freq_day_number(length(confirmed1) - 29))
best_model <- model_list[[find_best_forecast_model(model_list, test)]]
summary(best_model)

#prognozowanie z wykorzystaniem najlepszego modelu
forecast <- forecast(best_model, h = 30)
show(calculate_ex_post_errors(forecast, test))

rm(train, n_seas_diffs, n_first_diffs, train_diff)
rm(orders_cand, fit_list, i)
rm(best_model_aicc, best_model_training_errors, auto_search, best_model)
rm(model_list, test, forecast)



