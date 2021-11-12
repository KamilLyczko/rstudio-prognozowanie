#szeregi dla 1 fali: train=05.03.2020-05.08.2020, test=06.08.2020-04.09.2020
#szeregi dla 2 fali: train=05.03.2020-20.10.2020, test=21.10.2020-19.11.2020
#szeregi dla 3 fali: train=05.03.2020-15.03.2021, test=16.03.2021-14.04.2021
#szeregi dla 4 fali: train=05.03.2020-25.09.2021, test=26.09.2021-25.10.2021

label_size <- 10
title_size <- 11
model_list <- list()
forecasts <- list()
scatterplots <- list()
forecasts_plots <- list()
test_comparison_plots <- list()
for(i in 1:length(c_time_series)) {
  c_train <- window(c_time_series[[i]], end = weekly_freq_day_number(length(c_time_series[[i]]) - 30))
  c_test <- window(c_time_series[[i]], start = weekly_freq_day_number(length(c_time_series[[i]]) - 29))
  t_train <- window(t_time_series_cleaned[[i]], 
                    end = weekly_freq_day_number(length(t_time_series_cleaned[[i]]) - 30))
  t_test <- window(t_time_series_cleaned[[i]],
                   start = weekly_freq_day_number(length(t_time_series_cleaned[[i]]) - 29))
  dates <- paste0("(", format(get_date_of_obs_ts(get_index_of_obs(c_train, 1)), "%d.%m.%Y"), "-",
                  format(get_date_of_obs_ts(get_index_of_obs(c_train, length(c_train))), "%d.%m.%Y"), ")")
  plot1_title <- paste0("Wykres rozrzutu wartości obserwacji szeregów czasowych dla ",i, " fali ", dates)
  plot2_title <- paste0("Wykres czasowy prognoz liczby zakażeń dla ", i, " fali - regresja liniowa")
  plot3_title <- paste0("Porównanie prognoz liczby zakażeń z szeregiem testowym dla ", i, 
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

rm(label_size, title_size, model_list, forecasts, i, c_train, t_train, c_test, t_test)
rm(scatterplots, forecasts_plots, test_comparison_plots)
rm(plot1_title, plot2_title, plot3_title, dates)
