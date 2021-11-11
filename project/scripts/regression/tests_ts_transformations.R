tests1_reformed <- tests1
tests2_reformed <- tests2
tests3_reformed <- tests3
tests4_reformed <- tests4
t_time_series_reformed <- list(
  tests1_reformed, tests2_reformed,
  tests3_reformed, tests4_reformed
)

for(i in 1:length(t_time_series_reformed)) 
  t_time_series_reformed[[i]] <- replace_na_values(t_time_series_reformed[[i]])

t_time_series_reformed[[4]][531] <- t_time_series_reformed[[4]][531 - 7]

tests1_reformed <- t_time_series_reformed[[1]]
tests2_reformed <- t_time_series_reformed[[2]]
tests3_reformed <- t_time_series_reformed[[3]]
tests4_reformed <- t_time_series_reformed[[4]]

x_labs <- c(seq(data$date[1], data$date[length(data$date)], 50))
x_labs[length(x_labs) + 1] <- data$date[length(data$date)]
ggplot() +
  geom_line(data = data.frame(x = data$date, y = tests4_reformed),
            aes(x, y, colour = "po zmianach")) +
  geom_line(data = data.frame(x = data$date, y = tests4), 
            aes(x, y, colour = "przed zmianami")) + 
  scale_x_date(breaks = x_labs, labels = x_labs, date_labels = "%d-%m-%Y") +
  labs(title = "", x = "data", y = "liczba testów", color = "Szereg czasowy:") +
  theme(plot.title = element_text(size = 10), axis.text.x = element_markdown(angle = 45, hjust = 1))

rm(i, x_labs)
  

