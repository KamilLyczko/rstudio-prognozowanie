data7 <- data.frame(
  date = as.Date(covid_poland$date),
  confirmed = ts(
    data$confirmed,
    frequency = 7,
    start = c(1, 5)
  ),
  deaths = ts(
    data$deaths,
    frequency = 7,
    start = c(1, 5)
  ),
  tests = ts(
    data$tests,
    frequency = 7,
    start = c(1, 5)
  )
)
x_values <- c("Nie", "Pon", "Wto", "Œro", "Czw", "Pi¹", "Sob")
confirmed_time_plots <- list()
deaths_time_plots <- list()
confirmed_season_plots <- list()
deaths_season_plots <- list()
i <- 1
begin <- 1;
finish <- begin + 99
while(begin < length(data7[, 1])) {
  if(finish > length(data7[, 1]))
    finish = length(data7[, 1])
  confirmed_ts <- window(data7$confirmed, start = weekly_freq_day_number(begin), end = weekly_freq_day_number(finish))
  deaths_ts <- window(data7$deaths, start = weekly_freq_day_number(begin), end = weekly_freq_day_number(finish))
  title_time <- paste(data7$date[begin], " do ", data7$date[finish])
  confirmed_time_plots[[i]] <- autoplot(confirmed_ts, main = paste("Wykres czasowy zaka¿eñ od ", title_time),
    xlab = "tydzieñ", ylab = "liczba zaka¿eñ") +
    theme(plot.title = element_text(size = 10))
  deaths_time_plots[[i]] <- autoplot(deaths_ts, main = paste("Wykres czasowy œmierci od ", title_time),
    xlab = "tydzieñ", ylab = "liczba zaka¿eñ") +
    theme(plot.title = element_text(size = 10))
  confirmed_season_plots[[i]] <- ggseasonplot(confirmed_ts, year.labels = TRUE, year.labels.left = TRUE, season.labels = x_values) +
    theme(plot.title = element_text(size = 10)) +
    ggtitle(paste("Wykres sezonowy zaka¿eñ od ", title_time)) + xlab("dzieñ tygodnia") + ylab("liczba zachorowañ")
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

rm(begin, finish, i, x_values, title_time)
