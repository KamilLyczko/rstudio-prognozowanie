# for(i in 1:length(c_time_series)) {
#   cat("Numery obserwacji o wartości 0 w szeregu liczb zakażeń dla ", i, " fali:\n")
#   cat(get_indexes_of_zero_values(c_time_series[[i]]), "\n\n")
# }


#poszukiwanie najlepszych wariantów metod dla badanych szeregów czasowych
h = 30
forecasts <- list(list(), list(), list(), list())
for(i in 1:length(c_time_series)) {
  first_non_zero_index <- 
    max(get_indexes_of_zero_values(c_time_series[[i]])) + 1
  train <- window(c_time_series[[i]], 
                  start = 
                    weekly_freq_day_number(
                      first_non_zero_index),
                  end = 
                    weekly_freq_day_number(
                      length(c_time_series[[i]]) - 30))
  test <- window(c_time_series[[i]], 
                 start = 
                   weekly_freq_day_number(
                     length(c_time_series[[i]]) - 29),
                 end = 
                   weekly_freq_day_number(
                     length(c_time_series[[i]]) - 30 + h))
  forecasts[[i]][[1]] <- ses(train, h = h)
  holt_forecasts <- list(
    holt(train, h = h),
    holt(train, h = h, damped = TRUE)
  )
  forecasts[[i]][[2]] <- 
    holt_forecasts[[find_best_forecast(holt_forecasts, test)]]
  winters_forecasts <- list(
    hw(train, h = h, seasonal = "additive"),
    hw(train, h = h, seasonal = "additive", damped = TRUE),
    hw(train, h = h, seasonal = "multiplicative"),
    hw(train, h = h, seasonal = "multiplicative", damped = TRUE)
  )
  forecasts[[i]][[3]] <- 
    winters_forecasts[[find_best_forecast(winters_forecasts, test)]]
}

mes <- "Najlepsze warianty metod:\n\n"
for(i in 1:length(forecasts)) {
  mes <- paste0(mes, i, " fala:\n")
  mes <- paste0(mes, "Metoda Browna:\t\t", forecasts[[i]][[1]]$method, "\n")
  mes <- paste0(mes, "Metoda Holta:\t\t", forecasts[[i]][[2]]$method, "\n")
  mes <- paste0(mes, "Metoda Wintersa:\t", forecasts[[i]][[3]]$method, "\n\n")
}
cat(mes)


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
        set_titles_size(title_size, label_size),
      generate_forecast_plot(forecasts[[i]][[j]], plot2_title, "liczba zakażeń") +
        set_titles_size(title_size, label_size),
      generate_test_comparison_plot(test, forecasts[[i]][[j]], plot3_title, "liczba zakażeń") +
        set_titles_size(title_size, label_size)
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

for(i in 1:length(forecasts)){
  test <- window(c_time_series[[i]], start = weekly_freq_day_number(length(c_time_series[[i]]) - 29),
                 end = weekly_freq_day_number(length(c_time_series[[i]]) - 30 + h))
  dates <- as.Date(c(seq(get_date_of_obs_ts(get_index_of_obs(test, 1)),
                         get_date_of_obs_ts(get_index_of_obs(test, length(test))), 1)))
  x_labs <- c(seq(dates[1], dates[length(dates)], 5))
  if(x_labs[length(x_labs)] != dates[length(dates)])
    x_labs[length(x_labs) + 1] <- dates[length(dates)]
  plot <- ggplot(data = data.frame(x = dates, y = test), 
                 aes(x, y, colour = "Wartości rzeczywiste")) +
    geom_line() +
    scale_x_date(breaks = x_labs, labels = x_labs, date_labels = "%d-%m-%Y") +
    labs(title = paste0("Wykres dopasowania prognoz do szeregu testowego liczb zakażeń ",
                        i, " fali"), x = "data", y ="liczba zakażeń", color = "") +
    theme(axis.text.x = element_markdown(angle = 45, hjust = 1)) +
    geom_line(data = data.frame(x = dates, y = forecasts[[i]][[1]]$mean), 
              aes(x, y, colour = forecasts[[i]][[1]]$method)) +
    geom_line(data = data.frame(x = dates, y = forecasts[[i]][[2]]$mean), 
              aes(x, y, colour = forecasts[[i]][[2]]$method)) + 
    geom_line(data = data.frame(x = dates, y = forecasts[[i]][[3]]$mean), 
              aes(x, y, colour = forecasts[[i]][[3]]$method))
  show(plot)
}


#wybór najlepszych metod
best_methods <- list()
for(i in 1:length(forecasts)) {
  test <- 
    window(c_time_series[[i]], 
           start = weekly_freq_day_number(
             length(c_time_series[[i]]) - 29),
           end = weekly_freq_day_number(
             length(c_time_series[[i]]) - 30 + 
                      length(forecasts[[i]][[1]]$mean)))
  best_methods[[i]] <- 
    forecasts[[i]][[find_best_forecast(forecasts[[i]], test)]]
}

mes <- "Najlepsze metody:\n"
for(i in 1:length(best_methods)){
  mes <- paste0(mes, i, " fala:\t", best_methods[[i]]$method, " - ", 
                get_methods_ets_model(best_methods[[i]]), "\n")
}
cat(mes)

for(i in 1:length(best_methods)) {
  test <- window(c_time_series[[i]], start = weekly_freq_day_number(length(c_time_series[[i]]) - 29),
                 end = weekly_freq_day_number(length(c_time_series[[i]]) - 30 + h))
  plot1_title <- paste0("Wykres dopasowania modelu do szeregu liczb zakażeń dla ",
                        i, " fali - ", best_methods[[i]]$method)
  plot2_title <- paste0("Wykres prognoz liczb zakażeń dla ", i, " fali")
  plot3_title <- paste0("Porównanie prognoz z rzeczywistymi liczbami zakażeń dla ", i, " fali")
  plots <- list(
    generate_fit_plot(best_methods[[i]], plot1_title, "liczba zakażeń") +
      set_titles_size(title_size, label_size),
    generate_forecast_plot(best_methods[[i]], plot2_title, "liczba zakażeń") +
      set_titles_size(title_size, label_size),
    generate_test_comparison_plot(test, best_methods[[i]], plot3_title, "liczba zakażeń") +
      set_titles_size(title_size, label_size)
  )
  grid.arrange(grobs = plots, ncol = 1)
  save_forecasts_to_csv(best_methods[[i]], 
                        paste0("c", i, "_best_forecasts.csv"), 
                        "exponential_smoothing/methods")
  save_df_to_csv(calculate_ex_post_errors(best_methods[[i]], test),
                 paste0("c", i, "_best_errors.csv"),
                 "exponential_smoothing/methods")
}

#Najlepsze:
#1 fala: ETS(MAdM) Damped Holt-Winters' multiplicative method
#2 fala: ETS(MAdM) Damped Holt-Winters' multiplicative method
#3 fala: ETS(MAM) Holt-Winters' multiplicative method
#4 fala: ETS(MAM) Holt-Winters' multiplicative method

rm(h, forecasts, i, j, first_non_zero_index, train, test)
rm(holt_forecasts, winters_forecasts, plot_methods, file_methods)
rm(label_size, title_size, plot1_title, plot2_title, plot3_title, plots)
rm(dates, x_labs, plot, mes)
rm(best_methods)