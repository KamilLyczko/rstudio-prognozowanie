label_size <- 10
title_size <- 11
model_list <- list()
forecasts <- list()
scatterplots <- list()
forecasts_plots <- list()
test_comparison_plots <- list()
c_time_series_short <- list(
  window(c_time_series[[1]], start = weekly_freq_day_number(95)),
  window(c_time_series[[2]], start = weekly_freq_day_number(190)),
  window(c_time_series[[4]], start = weekly_freq_day_number(500))
)
t_time_series_cleaned_short <- list(
  window(t_time_series_cleaned[[1]], start = weekly_freq_day_number(95)),
  window(t_time_series_cleaned[[2]], start = weekly_freq_day_number(190)),
  window(t_time_series_cleaned[[4]], start = weekly_freq_day_number(500))
)
offset <- c(
  length(c_time_series[[1]]) - length(c_time_series_short[[1]]),
  length(c_time_series[[2]]) - length(c_time_series_short[[2]]),
  length(c_time_series[[4]]) - length(c_time_series_short[[3]])
)
for(i in 1:length(c_time_series_short)) {
  c_train <- window(c_time_series_short[[i]], end = weekly_freq_day_number(length(c_time_series_short[[i]]) - 30 + offset[i]))
  c_test <- window(c_time_series_short[[i]], start = weekly_freq_day_number(length(c_time_series_short[[i]]) - 29 + offset[i]))
  t_train <- window(t_time_series_cleaned_short[[i]], 
                    end = weekly_freq_day_number(length(t_time_series_cleaned_short[[i]]) - 30 + offset[i]))
  t_test <- window(t_time_series_cleaned_short[[i]],
                   start = weekly_freq_day_number(length(t_time_series_cleaned_short[[i]]) - 29 + offset[i]))
  dates <- paste0("(", format(get_date_of_obs_ts(get_index_of_obs(c_train, 1)), "%d.%m.%Y"), "-",
                  format(get_date_of_obs_ts(get_index_of_obs(c_train, length(c_train))), "%d.%m.%Y"), ")")
  wave_num <- i
  if(wave_num == 3)
    wave_num <- 4
  plot1_title <- paste0("Wykres rozrzutu wartości obserwacji szeregów czasowych dla ",wave_num, " fali ", dates)
  plot2_title <- paste0("Wykres czasowy prognoz liczby zakażeń dla ", wave_num, " fali - regresja liniowa")
  plot3_title <- paste0("Porównanie prognoz liczby zakażeń z szeregiem testowym dla ", wave_num, 
                        " fali - regresja liniowa")
  scatterplots[[i]] <- ggplot(data = data.frame(x = t_train, y = c_train), aes(x, y)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, show.legend = TRUE) +
    labs(title = plot1_title, x = "liczba testów", y = "liczba zakażeń") +
    theme(axis.title.x = element_text(size = label_size), 
          axis.title.y = element_text(size = label_size),
          plot.title = element_text(size = title_size))
  model_list[[i]] <- tslm(c_train ~ t_train)
  forecasts[[i]] <- forecast(model_list[[i]], newdata = data.frame(t_train = t_test))
  forecasts_plots[[i]] <- autoplot(c_train) + 
    autolayer(forecasts[[i]]) +
    ggtitle(plot2_title) +
    xlab("numer tygodnia") + ylab("liczba zakażeń") +
    theme(axis.title.x = element_text(size = label_size), 
          axis.title.y = element_text(size = label_size),
          plot.title = element_text(size = title_size))
  test_comparison_plots[[i]] <- generate_test_comparison_plot(c_test, forecasts[[i]],
                                                              title = plot3_title,
                                                              ylab = "liczba zakażeń") +
    theme(axis.title.x = element_text(size = label_size), 
          axis.title.y = element_text(size = label_size),
          plot.title = element_text(size = title_size))
}
grid.arrange(grobs = scatterplots, ncol = 1)
grid.arrange(grobs = forecasts_plots, ncol = 1)
grid.arrange(grobs = test_comparison_plots, ncol = 1)

rm(c_time_series_short, t_time_series_cleaned_short)
rm(label_size, title_size, model_list, forecasts, i, c_train, t_train, c_test, t_test)
rm(scatterplots, forecasts_plots, test_comparison_plots)
rm(plot1_title, plot2_title, plot3_title, dates, wave_num)