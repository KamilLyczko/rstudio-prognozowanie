x_values <- c("Nie", "Pon", "Wto", "Œro", "Czw", "Pi¹", "Sob")
confirmed_time_plots <- list()
deaths_time_plots <- list()
confirmed_season_plots <- list()
deaths_season_plots <- list()
i <- 1
begin <- 1;
finish <- begin + 99
while(begin < length(data[, 1])) {
  if(finish > length(data[, 1]))
    finish = length(data[, 1])
  confirmed_ts <- window(confirmed, start = weekly_freq_day_number(begin), end = weekly_freq_day_number(finish))
  deaths_ts <- window(deaths, start = weekly_freq_day_number(begin), end = weekly_freq_day_number(finish))
  date_window <- window(data$date, start = begin, end = finish)
  x_labs <- c(seq(date_window[1], date_window[length(date_window)], 20))
  x_labs[length(x_labs) + 1] <- date_window[length(date_window)]
  title_time <- paste(data$date[begin], " do ", data$date[finish])
  confirmed_time_plots[[i]] <- ggplot(data = data.frame(x = date_window, y = confirmed_ts), aes(x, y)) +
    geom_line() +
    scale_x_date(breaks = x_labs, labels = x_labs, date_labels = "%d-%m-%Y") +
    labs(title = paste("Wykres czasowy zaka¿eñ od ", title_time), x = "Data", y = "Liczba zaka¿eñ") +
    theme(plot.title = element_text(size = 10), axis.text.x = element_markdown(angle = 45, hjust = 1))
  deaths_time_plots[[i]] <- ggplot(data = data.frame(x = date_window, y = deaths_ts), aes(x, y)) +
    geom_line() +
    scale_x_date(breaks = x_labs, labels = x_labs, date_labels = "%d-%m-%Y") +
    labs(title = paste("Wykres czasowy œmierci od ", title_time), x = "Data", y = "Liczba œmierci") +
    theme(plot.title = element_text(size = 10), axis.text.x = element_markdown(angle = 45, hjust = 1))
  confirmed_season_plots[[i]] <- ggseasonplot(confirmed_ts, year.labels = TRUE, year.labels.left = TRUE, season.labels = x_values) +
    theme(plot.title = element_text(size = 10)) +
    ggtitle(paste("Wykres sezonowy zaka¿eñ od ", title_time)) + xlab("dzieñ tygodnia") + ylab("liczba zaka¿eñ")
  deaths_season_plots[[i]] <- ggseasonplot(deaths_ts, year.labels = TRUE, year.labels.left = TRUE, season.labels = x_values) +
    theme(plot.title = element_text(size = 10)) +
    ggtitle(paste("Wykres sezonowy œmierci od ", title_time)) + xlab("dzieñ tygodnia") + ylab("liczba œmierci")
  begin <- finish + 1
  finish <- begin + 99
  i <- i + 1
}
grid.arrange(grobs = confirmed_time_plots, ncol = 2)
grid.arrange(grobs = deaths_time_plots, ncol = 2)
grid.arrange(grobs = confirmed_season_plots, ncol = 2)
grid.arrange(grobs = deaths_season_plots, ncol = 2)

rm(begin, finish, i, x_values, title_time, confirmed_time_plots, confirmed_season_plots, deaths_time_plots, deaths_season_plots,
   confirmed_ts, deaths_ts, date_window, x_labs)
