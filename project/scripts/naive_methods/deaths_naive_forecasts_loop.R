deaths_forecasts <- list()
for(i in 1:length(d_time_series)) {
  train <- window(d_time_series[[i]], end = weekly_freq_day_number(length(d_time_series[[i]]) - 30))
  test <- window(d_time_series[[i]], start = weekly_freq_day_number(length(d_time_series[[i]]) - 29))
  
  deaths_forecasts[[i]] <- naive_forecasts(train, 30)
  
  plot_title1 <- paste("Prognozy metody naiwnej liczby śmierci dla", i, "fali")
  plot_title2 <- paste("Porównanie prognoz liczby śmierci z szeregiem testowym dla", i, "fali")
  if(i != 4)
    plot1 <- generate_naive_forecasts_plot2(train, deaths_forecasts[[i]], plot_title1, "liczba śmierci")
  else
    plot1 <- generate_naive_forecasts_plot2(window(train, start = weekly_freq_day_number(500)), 
                                       deaths_forecasts[[i]], plot_title1, "liczba śmierci")
  plot2 <- generate_naive_test_comparison_plot2(test, deaths_forecasts[[i]], plot_title2, "liczba śmierci")
  grid.arrange(grobs = list(plot1, plot2), ncol = 1)
  errors_naive <- calculate_ex_post_errors(deaths_forecasts[[i]]$naive, test)
  errors_snaive <- calculate_ex_post_errors(deaths_forecasts[[i]]$snaive, test)
  errors_drift <- calculate_ex_post_errors(deaths_forecasts[[i]]$drift, test)
  errors <- rbind(errors_naive, errors_snaive, errors_drift)
  save_df_to_csv(errors, paste0("d", i, "_naive_methods_errors.csv"), "naive_methods")
  cat("Mierniki trafności prognoz wygasłych liczb śmierci dla", i, "fali\n")
  cat("prosta metoda naiwna:\n")
  show(errors_naive)
  cat("sezonowa metoda naiwna:\n")
  show(errors_snaive)
  cat("przyrostowa metoda naiwna:\n")
  show(errors_drift)
}

rm(train, test, plot_title1, plot_title2, i, deaths_forecasts, plot1, plot2)
rm(errors_naive, errors_snaive, errors_drift, errors)