#algorytm poszukiwania najlepszego modelu dla szeregów czasowych 1, 2 i 4 fali
#wyznacznikiem jest miernik MAPE trafności prognoz wygasłych dla horyzontu 30 dni
#tworzenie modeli dla szeregów czasowych o różnej długości

begin_list <- list(
  c(1, 1, 1, 1), c(50, 50, 50, 50), c(60, 60, 60, 60),c(70, 70, 70, 70), 
  c(90, 90, 90, 90), c(110, 110, 110, 110), c(80, 130, 130, 130),
  c(100, 160, 160, 160), c(75, 180, 180, 180), c(85, 200, 200, 200), 
  c(95, 145, 250, 300), c(105, 170, 300, 400), c(30, 190, 320, 500)
)

MAPE_matrix1 <- matrix(nrow = length(begin_list), ncol = 4)
MAPE_matrix2 <- matrix(nrow = length(begin_list), ncol = 4)
MAPE_matrix3 <- matrix(nrow = length(begin_list), ncol = 4)
MAPE_matrix4 <- matrix(nrow = length(begin_list), ncol = 4)
MAPE_matrix5 <- matrix(nrow = length(begin_list), ncol = 4)

for(i in 1:length(begin_list)) {
  c_time_series_short <- list(
    window(c_time_series[[1]], start = weekly_freq_day_number(begin_list[[i]][1])),
    window(c_time_series[[2]], start = weekly_freq_day_number(begin_list[[i]][2])),
    window(c_time_series[[3]], start = weekly_freq_day_number(begin_list[[i]][3])),
    window(c_time_series[[4]], start = weekly_freq_day_number(begin_list[[i]][4]))
  )
  t_time_series_cleaned_short <- list(
    window(t_time_series_cleaned[[1]], start = weekly_freq_day_number(begin_list[[i]][1])),
    window(t_time_series_cleaned[[2]], start = weekly_freq_day_number(begin_list[[i]][2])),
    window(t_time_series_cleaned[[3]], start = weekly_freq_day_number(begin_list[[i]][3])),
    window(t_time_series_cleaned[[4]], start = weekly_freq_day_number(begin_list[[i]][4]))
  )
  offset <- c(
    length(c_time_series[[1]]) - length(c_time_series_short[[1]]),
    length(c_time_series[[2]]) - length(c_time_series_short[[2]]),
    length(c_time_series[[3]]) - length(c_time_series_short[[3]]),
    length(c_time_series[[4]]) - length(c_time_series_short[[4]])
  )
  for(j in 1:length(c_time_series_short)) {
    c_train <- window(c_time_series_short[[j]], end = weekly_freq_day_number(length(c_time_series_short[[j]]) - 30 + offset[j]))
    c_test <- window(c_time_series_short[[j]], start = weekly_freq_day_number(length(c_time_series_short[[j]]) - 29 + offset[j]))
    t_train <- window(t_time_series_cleaned_short[[j]], 
                      end = weekly_freq_day_number(length(t_time_series_cleaned_short[[j]]) - 30 + offset[j]))
    t_test <- window(t_time_series_cleaned_short[[j]],
                     start = weekly_freq_day_number(length(t_time_series_cleaned_short[[j]]) - 29 + offset[j]))
    model1 <- tslm(c_train ~ t_train)
    predictions1 <- forecast(model1, newdata = data.frame(t_train = t_test))
    MAPE_matrix1[i, j] <- calculate_ex_post_errors(predictions1, c_test)$MAPE
    model2 <- tslm(c_train ~ season)
    predictions2 <- forecast(model2, h = length(c_test))
    MAPE_matrix2[i, j] <- calculate_ex_post_errors(predictions2, c_test)$MAPE
    model3 <- tslm(c_train ~ trend + season)
    predictions3 <- forecast(model3, h = length(c_test))
    MAPE_matrix3[i, j] <- calculate_ex_post_errors(predictions3, c_test)$MAPE
    model4 <- tslm(c_train ~ t_train + season)
    predictions4 <- forecast(model4, newdata = data.frame(t_train = t_test))
    MAPE_matrix4[i, j] <- calculate_ex_post_errors(predictions4, c_test)$MAPE
    model5 <- tslm(c_train ~ t_train + trend + season)
    predictions5 <- forecast(model5, newdata = data.frame(t_train = t_test))
    MAPE_matrix5[i, j] <- calculate_ex_post_errors(predictions5, c_test)$MAPE
  }
}
cat("Najlepsze wyniki (najniższe wartości MAPE):\n")
cat("Model: liczba zakażeń = f(liczba testów) + e\n")
find_best_first_obs_numb(begin_list, MAPE_matrix1)
cat("Model: liczba zakażeń = f(season) + e\n")
find_best_first_obs_numb(begin_list, MAPE_matrix2)
cat("Model: liczba zakażeń = f(trend, season) + e\n")
find_best_first_obs_numb(begin_list, MAPE_matrix3)
cat("Model: liczba zakażeń = f(liczba testów, season) + e\n")
find_best_first_obs_numb(begin_list, MAPE_matrix4)
cat("Model: liczba zakażeń = f(liczba testów, trend, season) + e\n")
find_best_first_obs_numb(begin_list, MAPE_matrix5)

#najlepsze wyniki:
#1 fala: model=f(trend, season) + e, first_obs=105
#2 fala: model=f(trend, season) + e, first_obs=200
#3 fala: model=f(liczba testów, trend, season) + e, first_obs=160
#4 fala: model=f(liczba testów, trend, season) + e, first_obs=500

rm(begin_list, c_time_series_short, t_time_series_cleaned_short, offset)
rm(c_train, c_test,t_train, t_test, i, j)
rm(model1, model2, model3, model4, model5)
rm(predictions1, predictions2, predictions3, predictions4, predictions5)
rm(MAPE_matrix1, MAPE_matrix2, MAPE_matrix3, MAPE_matrix4, MAPE_matrix5)