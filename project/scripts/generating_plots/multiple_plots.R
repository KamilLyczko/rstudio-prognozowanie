x_values <- c("Nie", "Pon", "Wto", "Śro", "Czw", "Pią", "Sob")
confirmed_time_plots <- list()
deaths_time_plots <- list()
tests_time_plots <- list()
confirmed_season_plots <- list()
deaths_season_plots <- list()
tests_season_plots <- list()
title_size <- 10
i <- 1
begin <- 1;
finish <- begin + 99
while(begin < length(data[, 1])) {
  if(finish > length(data[, 1]))
    finish = length(data[, 1])
  confirmed_ts <- window(confirmed, start = weekly_freq_day_number(begin), end = weekly_freq_day_number(finish))
  deaths_ts <- window(deaths, start = weekly_freq_day_number(begin), end = weekly_freq_day_number(finish))
  tests_ts <- window(tests, start = weekly_freq_day_number(begin), end = weekly_freq_day_number(finish))
  date_window <- window(data$date, start = begin, end = finish)
  x_labs <- c(seq(date_window[1], date_window[length(date_window)], 20))
  x_labs[length(x_labs) + 1] <- date_window[length(date_window)]
  title_time <- paste(data$date[begin], " do ", data$date[finish])
  confirmed_time_plots[[i]] <- ggplot(data = data.frame(x = date_window, y = confirmed_ts), aes(x, y)) +
    geom_line() +
    scale_x_date(breaks = x_labs, labels = x_labs, date_labels = "%d-%m-%Y") +
    labs(title = paste("Wykres czasowy liczby zakażeń od ", title_time), x = "data", y = "liczba zakażeń") +
    theme(plot.title = element_text(size = title_size), axis.text.x = element_markdown(angle = 45, hjust = 1))
  deaths_time_plots[[i]] <- ggplot(data = data.frame(x = date_window, y = deaths_ts), aes(x, y)) +
    geom_line() +
    scale_x_date(breaks = x_labs, labels = x_labs, date_labels = "%d-%m-%Y") +
    labs(title = paste("Wykres czasowy liczby śmierci od ", title_time), x = "data", y = "liczba śmierci") +
    theme(plot.title = element_text(size = title_size), axis.text.x = element_markdown(angle = 45, hjust = 1))
  tests_time_plots[[i]] <- ggplot(data = data.frame(x = date_window, y = tests_ts), aes(x, y)) +
    geom_line() +
    scale_x_date(breaks = x_labs, labels = x_labs, date_labels = "%d-%m-%Y") +
    labs(title = paste("Wykres czasowy liczby testów od ", title_time), x = "data", y = "liczba testów") +
    theme(plot.title = element_text(size = title_size), axis.text.x = element_markdown(angle = 45, hjust = 1))
  confirmed_season_plots[[i]] <- ggseasonplot(confirmed_ts, year.labels = TRUE, year.labels.left = TRUE, season.labels = x_values) +
    theme(plot.title = element_text(size = title_size)) +
    ggtitle(paste("Wykres sezonowy liczby zakażeń od ", title_time)) + xlab("dzień tygodnia") + ylab("liczba zakażeń")
  deaths_season_plots[[i]] <- ggseasonplot(deaths_ts, year.labels = TRUE, year.labels.left = TRUE, season.labels = x_values) +
    theme(plot.title = element_text(size = title_size)) +
    ggtitle(paste("Wykres sezonowy liczby śmierci od ", title_time)) + xlab("dzień tygodnia") + ylab("liczba śmierci")
  tests_season_plots[[i]] <- ggseasonplot(tests_ts, year.labels = TRUE, year.labels.left = TRUE, season.labels = x_values) +
    theme(plot.title = element_text(size = title_size)) +
    ggtitle(paste("Wykres sezonowy liczby testów od ", title_time)) + xlab("dzień tygodnia") + ylab("liczba testów")
  begin <- finish + 1
  finish <- begin + 99
  i <- i + 1
}
grid.arrange(grobs = confirmed_time_plots, ncol = 2)
grid.arrange(grobs = deaths_time_plots, ncol = 2)
grid.arrange(grobs = tests_time_plots, ncol = 2)
grid.arrange(grobs = confirmed_season_plots, ncol = 2)
grid.arrange(grobs = deaths_season_plots, ncol = 2)
grid.arrange(grobs = tests_season_plots, ncol = 2)

rm(begin, finish, i, x_values, title_time, confirmed_time_plots, confirmed_season_plots, deaths_time_plots, deaths_season_plots,
   confirmed_ts, deaths_ts, date_window, x_labs, tests_time_plots, tests_season_plots, tests_ts, title_size)
