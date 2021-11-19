# for(i in 1:length(c_time_series)) {
#   cat("Numery obserwacji o wartości 0 w szeregu liczb zakażeń dla ", i, " fali:\n")
#   cat(get_indexes_of_zero_values(c_time_series[[i]]), "\n\n")
# }


#poszukiwanie najlepszych wariantów metod dla badanych szeregów czasowych
h = 30
forecasts <- list(list(), list(), list(), list())
for(i in 1:length(c_time_series)) {
  first_non_zero_index <- max(get_indexes_of_zero_values(c_time_series[[i]])) + 1
  train <- window(c_time_series[[i]], start = weekly_freq_day_number(first_non_zero_index),
                  end = weekly_freq_day_number(length(c_time_series[[i]]) - 30))
  test <- window(c_time_series[[i]], start = weekly_freq_day_number(length(c_time_series[[i]]) - 29),
                 end = weekly_freq_day_number(length(c_time_series[[i]]) - 30 + h))
  forecasts[[i]][[1]] <- ses(train, h = h)
  holt_forecasts <- list(
    holt(train, h = h),
    holt(train, h = h, damped = TRUE)
  )
  forecasts[[i]][[2]] <- holt_forecasts[[find_best_forecast(holt_forecasts, test)]]
  winters_forecasts <- list(
    hw(train, h = h, seasonal = "additive"),
    hw(train, h = h, seasonal = "additive", damped = TRUE),
    hw(train, h = h, seasonal = "multiplicative"),
    hw(train, h = h, seasonal = "multiplicative", damped = TRUE)
  )
  forecasts[[i]][[3]] <- winters_forecasts[[find_best_forecast(winters_forecasts, test)]]
}

#wizualizacja prognoz i zapis do pliku
plot_methods <- c(
  "metoda Browna",
  "metoda Holta",
  "metoda Wintersa"
)
file_methods <- c("_brown", "_holt", "_winters")
label_size <- 10
title_size <- 11
for(i in 1:length(forecasts)) {
  test <- window(c_time_series[[i]], start = weekly_freq_day_number(length(c_time_series[[i]]) - 29),
                 end = weekly_freq_day_number(length(c_time_series[[i]]) - 30 + h))
  for(j in 1:length(forecasts[[i]])) {
    plot1_title <- paste0("Wykres dopasowania modelu do szeregu liczb zakażeń dla ",
                          i, " fali - ", plot_methods[j], " (", forecasts[[i]][[j]]$method, ")")
    plot2_title <- paste0("Wykres prognoz liczb zakażeń dla ", i, " fali")
    plot3_title <- paste0("Porównanie prognoz z rzeczywistymi liczbami zakażeń dla ", i, " fali")
    plots <- list(
      generate_fit_plot(forecasts[[i]][[j]], plot1_title, "liczba zakażeń") +
        theme(axis.title.x = element_text(size = label_size), 
              axis.title.y = element_text(size = label_size),
              plot.title = element_text(size = title_size)),
      generate_forecast_plot(forecasts[[i]][[j]], plot2_title, "liczba zakażeń") +
        theme(axis.title.x = element_text(size = label_size), 
              axis.title.y = element_text(size = label_size),
              plot.title = element_text(size = title_size)),
      generate_test_comparison_plot(test, forecasts[[i]][[j]], plot3_title, "liczba zakażeń") +
        theme(axis.title.x = element_text(size = label_size), 
              axis.title.y = element_text(size = label_size),
              plot.title = element_text(size = title_size))
    )
    grid.arrange(grobs = plots, ncol = 1)
    save_forecasts_to_csv(forecasts[[i]][[j]], 
                          paste0("c", i, file_methods[j], "_forecasts_h", h, ".csv"), 
                          "exponential_smoothing/methods")
    save_df_to_csv(calculate_ex_post_errors(forecasts[[i]][[j]], test),
                   paste0("c", i, file_methods[j], "_errors_h", h, ".csv"),
                   "exponential_smoothing/methods")
  }
}

#wybór najlepszych metod
best_methods <- list()
for(i in 1:length(forecasts)) {
  test <- window(c_time_series[[i]], start = weekly_freq_day_number(length(c_time_series[[i]]) - 29),
                 end = weekly_freq_day_number(length(c_time_series[[i]]) - 30 + 
                                                length(forecasts[[i]][[1]]$mean)))
  best_methods[[i]] <- forecasts[[i]][[find_best_forecast(forecasts[[i]], test)]]
}

cat("Najlepsze metody:\n")
for(i in 1:length(best_methods))
  cat(i, "fala:\t", best_methods[[i]]$method, " - ", get_methods_ets_model(best_methods[[i]]), "\n")

#Najlepsze:
#1 fala: ETS(MAdM) Damped Holt-Winters' multiplicative method
#2 fala: ETS(MAdM) Damped Holt-Winters' multiplicative method
#3 fala: ETS(MAM) Holt-Winters' multiplicative method
#4 fala: ETS(MAM) Holt-Winters' multiplicative method

rm(h, forecasts, i, j, first_non_zero_index, train, test)
rm(holt_forecasts, winters_forecasts, plot_methods, file_methods)
rm(label_size, title_size, plot1_title, plot2_title, plot3_title, plots)
rm(best_methods)