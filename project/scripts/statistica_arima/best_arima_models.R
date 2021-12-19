#automatyczne poszukiwanie najlepszych modeli dla szeregów używanych w programie Statistica

#---------------------------liczba zachorowań-----------------------------
#liczba zachorowań - 1 fala
train <- window(confirmed1, 
                end = weekly_freq_day_number(length(confirmed1) - 30)) + 1
model <- auto.arima(train, lambda = 0)
summary(model) #ARIMA(1,2,1)(2,0,0)[7] 
autoplot(forecast(model, h = 30), PI = FALSE) + autolayer(fitted(model))

#liczba zachorowań - 2 fala
train <- window(confirmed2, 
                end = weekly_freq_day_number(length(confirmed2) - 30)) + 1
model <- auto.arima(train, lambda = 0)
summary(model) #ARIMA(2,2,4)(2,0,0)[7]
autoplot(forecast(model, h = 30), PI = FALSE) + autolayer(fitted(model))

#liczba zachorowań - 3 fala
train <- window(confirmed3, 
                end = weekly_freq_day_number(length(confirmed3) - 30)) + 1
model <- auto.arima(train, lambda = 0)
summary(model) #ARIMA(4,1,3)(2,1,2)[7]
autoplot(forecast(model, h = 30), PI = FALSE) + autolayer(fitted(model))

#liczba zachorowań - 4 fala
train <- window(confirmed4, 
                end = weekly_freq_day_number(length(confirmed4) - 30)) + 1
model <- auto.arima(train, lambda = 0)
summary(model) #ARIMA(3,1,5)(2,1,1)[7] 
autoplot(forecast(model, h = 30), PI = FALSE) + autolayer(fitted(model))

#---------------------------liczba śmierci-------------------------------
#liczba śmierci - 1 fala
train <- window(deaths1, start = weekly_freq_day_number(24),
                end = weekly_freq_day_number(length(deaths1) - 30)) + 1
model <- auto.arima(train, lambda = 0)
summary(model) #ARIMA(0,1,1)(1,0,0)[7] 
autoplot(forecast(model, h = 30), PI = FALSE) + autolayer(fitted(model))

#liczba śmierci - 2 fala
train <- window(deaths2, start = weekly_freq_day_number(24),
                end = weekly_freq_day_number(length(deaths2) - 30)) + 1
model <- auto.arima(train, lambda = 0)
summary(model) #ARIMA(2,1,2)(2,0,0)[7] with drift (constant)
autoplot(forecast(model, h = 30), PI = FALSE) + autolayer(fitted(model))

#liczba śmierci - 3 fala
train <- window(deaths3, start = weekly_freq_day_number(24),
                end = weekly_freq_day_number(length(deaths3) - 30)) + 1
model <- auto.arima(train, lambda = 0)
summary(model) #ARIMA(2,0,3)(1,1,2)[7]
autoplot(forecast(model, h = 30), PI = FALSE) + autolayer(fitted(model))

#liczba śmierci - 4 fala
train <- window(deaths4, start = weekly_freq_day_number(24),
                end = weekly_freq_day_number(length(deaths4) - 30)) + 1
model <- auto.arima(train, lambda = 0)
summary(model) #ARIMA(3,1,3)(2,1,2)[7] 
autoplot(forecast(model, h = 30), PI = FALSE) + autolayer(fitted(model))

#===============================================================================
c_models <- list()
d_models <- list()
for(i in 1:4){
  c_train <- window(c_time_series[[i]], 
                    end = weekly_freq_day_number(
                      length(c_time_series[[i]]) - 30)) + 1
  d_train <- window(d_time_series[[i]], 
                    start = weekly_freq_day_number(24),
                    end = weekly_freq_day_number(
                      length(d_time_series[[i]]) - 30)) + 1
  c_models[[i]] <- auto.arima(c_train, lambda = 0)
  d_models[[i]] <- auto.arima(d_train, lambda = 0)
}

mes <- 
  "Wybrane modele:\n\nSzeregi liczb zakażeń:\n"
for(i in 1:length(c_models)){
  mes <- paste0(mes, i, " fala:\t",
                get_arima_model_type(
                  c_models[[i]]), "\n")
}
mes <- paste0(mes, "\nSzeregi liczb śmierci:\n")
for(i in 1:length(d_models)){
  mes <- paste0(mes, i, " fala:\t",
                get_arima_model_type(
                  d_models[[i]]), "\n")
}
cat(mes)

rm(train, model)
rm(c_models, d_models, c_train, d_train, i, mes)
