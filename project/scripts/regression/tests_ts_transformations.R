t_time_series_cleaned <- t_time_series

for(i in 1:length(t_time_series_cleaned)) 
  t_time_series_cleaned[[i]] <- replace_na_values(t_time_series_cleaned[[i]])

t_time_series_cleaned[[4]][531] <- t_time_series_cleaned[[4]][531 - 7] #outlier

x_labs <- c(seq(data$date[1], data$date[length(data$date)], 50))
x_labs[length(x_labs) + 1] <- data$date[length(data$date)]
ggplot() +
  geom_line(data = data.frame(x = data$date, y = t_time_series_cleaned[[4]]),
            aes(x, y, colour = "po zmianach")) +
  geom_line(data = data.frame(x = data$date, y = t_time_series[[4]]), 
            aes(x, y, colour = "przed zmianami")) + 
  scale_x_date(breaks = x_labs, labels = x_labs, date_labels = "%d-%m-%Y") +
  labs(title = "", x = "data", y = "liczba testów", color = "Szereg czasowy:") +
  theme(plot.title = element_text(size = 10), axis.text.x = element_markdown(angle = 45, hjust = 1))

rm(i, x_labs)
  

