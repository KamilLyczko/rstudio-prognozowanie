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
  forecast_plots <- list()
  test_comparison_plots <- list()
  methods <- c(
    "metoda Browna",
    "metoda Holta",
    "metoda Wintersa"
  )
  plot1_title <- paste0("Wykres czasowy prognoz liczby zakażeń dla ", i, " fali")
  plot2_title <- paste0("Porównanie prognoz liczby zakażeń z szeregiem testowym dla ", i, " fali")
  for(j in 1:length(forecasts)) {
    if(i == 1 | i == 2) {
      forecast_plots[[j]] <- autoplot(train, series = "Wartości rzeczywiste") + 
        autolayer(forecasts[[j]], series = "Prognozy", PI = FALSE) +
        autolayer(fitted(forecasts[[j]]), series = "Wartości dopasowane") +
        guides(colour = guide_legend(title = "")) +
        ggtitle(paste0(plot1_title, " - ", methods[j])) +
        xlab("numer tygodnia") + ylab("liczba zakażeń") +
        theme(axis.title.x = element_text(size = label_size), 
              axis.title.y = element_text(size = label_size),
              plot.title = element_text(size = title_size))
    }
    else {
      forecast_plots[[j]] <- autoplot(window(train, start = weekly_freq_day_number(200)),
                                      series = "Wartości rzeczywiste") + 
        autolayer(forecasts[[j]], series = "Prognozy", PI = FALSE) +
        autolayer(window(fitted(forecasts[[j]]), start = weekly_freq_day_number(200)), 
                 series = "Wartości dopasowane") +
        guides(colour = guide_legend(title = "")) +
        ggtitle(paste0(plot1_title, " - ", methods[j])) +
        xlab("numer tygodnia") + ylab("liczba zakażeń") +
        theme(axis.title.x = element_text(size = label_size), 
              axis.title.y = element_text(size = label_size),
              plot.title = element_text(size = title_size))
    }
    
    test_comparison_plots[[j]] <- generate_test_comparison_plot(test, forecasts[[j]],
                                                           title = paste0(plot2_title, 
                                                                          " - ", 
                                                                          methods[j])) +
      theme(axis.title.x = element_text(size = label_size), 
            axis.title.y = element_text(size = label_size),
            plot.title = element_text(size = title_size))
  }
  grid.arrange(grobs = forecast_plots, ncol = 1)
  grid.arrange(grobs = test_comparison_plots, ncol = 1)
}

rm(label_size, title_size, train, test, forecasts, forecast_plots, test_comparison_plots)
rm(i, j, methods, plot1_title, plot2_title, first_non_zero_index)