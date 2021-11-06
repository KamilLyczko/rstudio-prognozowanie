deaths_forecasts <- list()
for(i in 1:length(d_time_series)) {
  train <- window(d_time_series[[i]], end = weekly_freq_day_number(length(d_time_series[[i]]) - 30))
  test <- window(d_time_series[[i]], start = weekly_freq_day_number(length(d_time_series[[i]]) - 29))
  
  deaths_forecasts[[i]] <- naive_forecasts(train, 30)
  
  plot_title1 <- paste("Prognozy metody naiwnej liczby œmierci dla", i, "fali")
  plot_title2 <- paste("Porównanie prognoz liczby œmierci z szeregiem testowym dla", i, "fali")
  if(i != 4)
    plot1 <- generate_naive_forecasts_plot2(train, deaths_forecasts[[i]], plot_title1, "liczba œmierci")
  else
    plot1 <- generate_naive_forecasts_plot2(window(train, start = weekly_freq_day_number(500)), 
                                       deaths_forecasts[[i]], plot_title1, "liczba œmierci")
  plot2 <- generate_naive_test_comparison_plot2(test, deaths_forecasts[[i]], plot_title2, "liczba œmierci")
  grid.arrange(grobs = list(plot1, plot2), ncol = 1)
}

rm(train, test, plot_title1, plot_title2, i, deaths_forecasts)