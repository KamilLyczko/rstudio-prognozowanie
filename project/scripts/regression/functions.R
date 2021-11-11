replace_na_values <- function(ts){
  ts_ind <- time(ts)[is.na(ts)] #157, 530
  for(i in 1:length(ts_ind)){
    ind <- calc_obs_num(calc_obs_week(ts_ind[i]))
    if((ind - 7) > 0)
      ts[ind] <- ts[ind - 7]
    else
      ts[ind] <- ts[ind - 1]
  }
  return(ts)
}

generate_test_comparison_plot <- function(test_ts, forecast_object, title = "", ylab = "") {
  dates <- as.Date(c(seq(get_date_of_obs_ts(get_index_of_obs(test_ts, 1)),
                         get_date_of_obs_ts(get_index_of_obs(test_ts, length(test_ts))), 1)))
  x_labs <- c(seq(dates[1], dates[length(dates)], 5))
  if(x_labs[length(x_labs)] != dates[length(dates)])
    x_labs[length(x_labs) + 1] <- dates[length(dates)]
  ggplot(data = data.frame(x = dates, y = test_ts), aes(x, y, colour = "Wartości rzeczywiste")) +
    geom_line() +
    geom_line(data = data.frame(x = dates, y = forecast_object$mean), 
              aes(x, y, colour = "Prognozy")) +
    scale_x_date(breaks = x_labs, labels = x_labs, date_labels = "%d-%m-%Y") +
    labs(title = title, x = "data", y = ylab, color = "") +
    theme(axis.text.x = element_markdown(angle = 45, hjust = 1))
}
