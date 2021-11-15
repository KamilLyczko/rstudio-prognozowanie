label_size <- 10
title_size <- 11
for(i in 1:length(c_time_series)) {
  first_non_zero_index <- max(get_indexes_of_zero_values(c_time_series[[i]])) + 1
  train <- window(c_time_series[[i]], start = weekly_freq_day_number(first_non_zero_index),
                  end = weekly_freq_day_number(length(c_time_series[[i]]) - 30))
  test <- window(c_time_series[[i]], start = weekly_freq_day_number(length(c_time_series[[i]]) - 29))
  forecasts <- list()
  forecasts[[1]] <- ses(train, h = 30) #brown
  forecasts[[2]] <- holt(train, h = 30) #holt
  forecasts[[3]] <- hw(train, seasonal = "multiplicative", h = 30) #winters
  fit_plots <- list()
  forecast_plots <- list()
  test_comparison_plots <- list()
  plot_methods <- c(
    "metoda Browna",
    "metoda Holta",
    "metoda Wintersa"
  )
  file_methods <- c("brown", "holt", "winters")
  plot1_title <- paste0("Wykres dopasowania modelu do danych rzeczywistych dla ", i, " fali")
  plot2_title <- paste0("Wykres czasowy prognoz liczby zakażeń dla ", i, " fali")
  plot3_title <- paste0("Porównanie prognoz liczby zakażeń z szeregiem testowym dla ", i, " fali")
  for(j in 1:length(forecasts)) {
    fit_plots[[j]] <- generate_fit_plot(forecasts[[j]], paste0(plot1_title, " - ", plot_methods[j]),
                                        "liczba zakażeń") +
      theme(axis.title.x = element_text(size = label_size), 
            axis.title.y = element_text(size = label_size),
            plot.title = element_text(size = title_size))
    if(i == 1 | i == 2) {
      forecast_plots[[j]] <- autoplot(train, series = "Wartości rzeczywiste") + 
        autolayer(forecasts[[j]], series = "Prognozy", PI = TRUE) +
        autolayer(fitted(forecasts[[j]]), series = "Wartości dopasowane") +
        coord_cartesian(ylim = c(0, max(forecasts[[j]]$x, forecasts[[j]]$mean))) +
        guides(colour = guide_legend(title = "")) +
        ggtitle(paste0(plot2_title, " - ", plot_methods[j])) +
        xlab("numer tygodnia") + ylab("liczba zakażeń") +
        theme(axis.title.x = element_text(size = label_size), 
              axis.title.y = element_text(size = label_size),
              plot.title = element_text(size = title_size))
    }
    else {
      forecast_plots[[j]] <- autoplot(window(train, start = weekly_freq_day_number(200)),
                                      series = "Wartości rzeczywiste") + 
        autolayer(forecasts[[j]], series = "Prognozy", PI = TRUE) +
        autolayer(window(fitted(forecasts[[j]]), start = weekly_freq_day_number(200)), 
                 series = "Wartości dopasowane") + 
        coord_cartesian(ylim = c(0, max(forecasts[[j]]$x, forecasts[[j]]$mean))) +
        guides(colour = guide_legend(title = "")) +
        ggtitle(paste0(plot2_title, " - ", plot_methods[j])) +
        xlab("numer tygodnia") + ylab("liczba zakażeń") +
        theme(axis.title.x = element_text(size = label_size), 
              axis.title.y = element_text(size = label_size),
              plot.title = element_text(size = title_size))
    }
    test_comparison_plots[[j]] <- generate_test_comparison_plot(test, forecasts[[j]],
                                                           title = paste0(plot3_title, 
                                                                          " - ", 
                                                                          plot_methods[j])) +
      theme(axis.title.x = element_text(size = label_size), 
            axis.title.y = element_text(size = label_size),
            plot.title = element_text(size = title_size))
    save_df_to_csv(calculate_ex_post_errors(forecasts[[j]], test),
                   paste0("c", i, "_", file_methods[j], "_errors.csv"), "exponential_smoothing")
  }
  grid.arrange(grobs = fit_plots, ncol = 1)
  grid.arrange(grobs = forecast_plots, ncol = 1)
  grid.arrange(grobs = test_comparison_plots, ncol = 1)
}

rm(label_size, title_size, train, test, forecasts, fit_plots, forecast_plots, test_comparison_plots)
rm(i, j, plot_methods, file_methods, plot1_title, plot2_title, plot3_title, first_non_zero_index)

