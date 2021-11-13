#najlepsze wyniki:
#1 fala: model=f(trend, season) + e, first_obs=105
#2 fala: model=f(trend, season) + e, first_obs=200
#3 fala: model=f(liczba testów, trend, season) + e, first_obs=160
#4 fala: model=f(liczba testów, trend, season) + e, first_obs=500

label_size <- 10
title_size <- 11
model_list <- list()
forecasts <- list()
scatterplots <- list()
forecasts_plots <- list()
test_comparison_plots <- list()
first_obs_numb <- c(105, 200, 160, 500)
c_time_series_short <- list(
  window(c_time_series[[1]], start = weekly_freq_day_number(first_obs_numb[1])),
  window(c_time_series[[2]], start = weekly_freq_day_number(first_obs_numb[2])),
  window(c_time_series[[3]], start = weekly_freq_day_number(first_obs_numb[3])),
  window(c_time_series[[4]], start = weekly_freq_day_number(first_obs_numb[4]))
)
t_time_series_cleaned_short <- list(
  window(t_time_series_cleaned[[1]], start = weekly_freq_day_number(first_obs_numb[1])),
  window(t_time_series_cleaned[[2]], start = weekly_freq_day_number(first_obs_numb[2])),
  window(t_time_series_cleaned[[3]], start = weekly_freq_day_number(first_obs_numb[3])),
  window(t_time_series_cleaned[[4]], start = weekly_freq_day_number(first_obs_numb[4]))
)
offset <- c(
  length(c_time_series[[1]]) - length(c_time_series_short[[1]]),
  length(c_time_series[[2]]) - length(c_time_series_short[[2]]),
  length(c_time_series[[3]]) - length(c_time_series_short[[3]]),
  length(c_time_series[[4]]) - length(c_time_series_short[[4]])
)
for(i in 1:length(c_time_series_short)) {
  c_train <- window(c_time_series_short[[i]], end = weekly_freq_day_number(length(c_time_series_short[[i]]) - 30 + offset[i]))
  c_test <- window(c_time_series_short[[i]], start = weekly_freq_day_number(length(c_time_series_short[[i]]) - 29 + offset[i]))
  t_train <- window(t_time_series_cleaned_short[[i]], 
                    end = weekly_freq_day_number(length(t_time_series_cleaned_short[[i]]) - 30 + offset[i]))
  t_test <- window(t_time_series_cleaned_short[[i]],
                   start = weekly_freq_day_number(length(t_time_series_cleaned_short[[i]]) - 29 + offset[i]))
  train_start <- format(get_date_of_obs_ts(get_index_of_obs(c_train, 1)), "%d.%m.%Y")
  train_finish <- format(get_date_of_obs_ts(get_index_of_obs(c_train, length(c_train))), "%d.%m.%Y")
  test_start <- format(get_date_of_obs_ts(get_index_of_obs(c_test, 1)), "%d.%m.%Y")
  test_finish <- format(get_date_of_obs_ts(get_index_of_obs(c_test, length(c_test))), "%d.%m.%Y")
  if(i == 1 | i == 2 ) {
    model_list[[i]] <- tslm(c_train ~ trend + season)
    forecasts[[i]] <- forecast(model_list[[i]], h = length(c_test))
  }
  else {
    model_list[[i]] <- tslm(c_train ~ t_train + trend + season)
    forecasts[[i]] <- forecast(model_list[[i]], newdata = data.frame(t_train = t_test))
    plot1_title <- paste0("Wykres rozrzutu wartości obserwacji szeregów czasowych dla ",i, " fali (", 
                          train_start, "-", train_finish, ")")
    scatterplots[[i - 2]] <- ggplot(data = data.frame(x = t_train, y = c_train), aes(x, y)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, show.legend = TRUE) +
      labs(title = plot1_title, x = "liczba testów", y = "liczba zakażeń") +
      theme(axis.title.x = element_text(size = label_size), 
            axis.title.y = element_text(size = label_size),
            plot.title = element_text(size = title_size))
  }
  plot2_title <- paste0("Wykres czasowy prognoz liczby zakażeń dla ", i, " fali - regresja liniowa (",
                        train_start, "-", train_finish, "; ",
                        test_start, "-", test_finish, ")")
  plot3_title <- paste0("Porównanie prognoz liczby zakażeń z szeregiem testowym dla ", i, 
                        " fali - regresja liniowa (", test_start, "-", test_finish, ")")  
  forecasts_plots[[i]] <- autoplot(c_train, series = "Wartości rzeczywiste") + 
    autolayer(forecasts[[i]], series = "Prognozy") +
    autolayer(fitted(forecasts[[i]]), series = "Wartości dopasowane") +
    guides(colour = guide_legend(title = "")) +
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
  
  save_forecasts_to_csv(forecasts[[i]], 
                        paste0("c", i, "_regression_forecasts.csv"), "regression")
  save_df_to_csv(calculate_ex_post_errors(forecasts[[i]], c_test), 
                 paste0("c", i, "_regression_errors.csv"), "regression")
}
grid.arrange(grobs = scatterplots, ncol = 1)
grid.arrange(grobs = forecasts_plots, ncol = 1)
grid.arrange(grobs = test_comparison_plots, ncol = 1)

rm(c_time_series_short, t_time_series_cleaned_short, first_obs_numb, offset)
rm(label_size, title_size, model_list, forecasts, i, c_train, t_train, c_test, t_test)
rm(scatterplots, forecasts_plots, test_comparison_plots)
rm(plot1_title, plot2_title, plot3_title)
rm(train_start, train_finish, test_start, test_finish)
