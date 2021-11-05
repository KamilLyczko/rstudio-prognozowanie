confirmed_forecasts <- list()
for(i in 1:length(c_time_series)) {
  train <- window(c_time_series[[i]], end = weekly_freq_day_number(length(c_time_series[[i]]) - 30))
  test <- window(c_time_series[[i]], start = weekly_freq_day_number(length(c_time_series[[i]]) - 29))
  
  confirmed_forecasts[[i]] <- naive_forecasts(train, 30)
  
  plot_title1 <- paste("Prognozy metody naiwnej liczby zaka¿eñ dla ", i, " fali")
  plot_title2 <- paste("Porównanie prognoz liczby zaka¿eñ z szeregiem testowym dla ", i, " fali")
  if(i != 4)
    show(generate_naive_forecasts_plot2(train, confirmed_forecasts[[i]], plot_title1, "liczba zaka¿eñ"))
  else
    show(generate_naive_forecasts_plot2(window(train, start = weekly_freq_day_number(500)), 
                                       confirmed_forecasts[[i]], plot_title1, "liczba zaka¿eñ"))
  show(generate_naive_test_comparison_plot2(test, confirmed_forecasts[[i]], plot_title2, "liczba zaka¿eñ"))
}

rm(train, test, plot_title1, plot_title2, i, confirmed_forecasts)