confirmed_scatterplots <- list()
label_size <- 10
title_size <- 11
i <- 1
begin <- 1;
finish <- begin + 99
while(begin < length(confirmed)) {
  if(finish > length(confirmed))
    finish = length(confirmed)
  confirmed_ts <- window(confirmed, start = weekly_freq_day_number(begin), end = weekly_freq_day_number(finish))
  tests_ts <- window(tests, start = weekly_freq_day_number(begin), end = weekly_freq_day_number(finish))
  begin_date <- data$date[begin]
  finish_date <- data$date[finish]
  title_time <- paste(begin_date, " do ", finish_date)
  title <- paste0("Wykres rozrzutu liczby zakażeń od liczby testów - od ", title_time)
  confirmed_scatterplots[[i]] <- ggplot(data = data.frame(x = tests_ts, y = confirmed_ts), 
                                        aes(x, y)) +
    geom_point() +
    labs(title = title, x = "liczba testów", y = "liczba zakażeń") +
    theme(axis.title.x = element_text(size = label_size), axis.title.y = element_text(size = label_size),
          plot.title = element_text(size = title_size))
  begin <- finish + 1
  finish <- begin + 99
  i <- i + 1
}

grid.arrange(grobs = confirmed_scatterplots, ncol = 2)

rm(confirmed_scatterplots, label_size, title_size, i, begin, finish)
rm(confirmed_ts, tests_ts, begin_date, finish_date)
rm(title_time, title_size, title)