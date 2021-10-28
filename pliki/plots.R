#wykresy czasowe
plot(confirmed1, main = "Wykres czasowy zaka¿eñ do I fali", xlab = "tydzieñ", ylab = "liczba zaka¿eñ")
plot(confirmed2, main = "Wykres czasowy zaka¿eñ do II fali", xlab = "tydzieñ", ylab = "liczba zaka¿eñ")
plot(confirmed3, main = "Wykres czasowy zaka¿eñ do III fali", xlab = "tydzieñ", ylab = "liczba zaka¿eñ")
plot(confirmed4, main = "Wykres czasowy zaka¿eñ do IV fali", xlab = "tydzieñ", ylab = "liczba zaka¿eñ")

x_labs <- c(seq(data$date[1], data$date[length(data$date)], 30))
x_labs[length(x_labs) + 1] <- data$date[length(data$date)]

ggplot(data = data, aes(date, confirmed)) + 
  geom_line() +
  labs(title = "Wykres czasowy liczby zaka¿eñ", x = "data", y = "liczba zaka¿eñ") +
  scale_x_date(breaks = x_labs, labels = x_labs, date_labels = "%d-%m-%Y") +
  theme(axis.text.x = element_markdown(angle = 45, hjust = 1))

ggplot(data = data, aes(date, deaths)) + 
  geom_line() +
  labs(title = "Wykres czasowy liczby œmierci", x = "data", y = "liczba œmierci") +
  scale_x_date(breaks = x_labs, labels = x_labs, date_labels = "%d-%m-%Y") +
  theme(axis.text.x = element_markdown(angle = 45, hjust = 1))

ggplot(data = data, aes(date, tests)) + 
  geom_line() +
  labs(title = "Wykres czasowy liczby testów", x = "data", y = "liczba testóW") +
  scale_x_date(breaks = x_labs, labels = x_labs, date_labels = "%d-%m-%Y") +
  theme(axis.text.x = element_markdown(angle = 45, hjust = 1))



#test_lab <- c(seq(data$date[1], data$date[600], 30))
#plot(data$date, confirmed, type = "l", xaxt = "n")
#axis(1, test_lab, format(test_lab, "%Y-%m-%d"), las = 2, cex.axis = 0.7)

plot(deaths1, main = "Wykres czasowy œmierci do I fali", xlab = "tydzieñ", ylab = "liczba œmierci")
plot(deaths2, main = "Wykres czasowy œmierci do II fali", xlab = "tydzieñ", ylab = "liczba œmierci")
plot(deaths3, main = "Wykres czasowy œmierci do III fali", xlab = "tydzieñ", ylab = "liczba œmierci")
plot(deaths4, main = "Wykres czasowy œmierci do IV fali", xlab = "tydzieñ", ylab = "liczba œmierci")

#wykresy sezonowe
x_values <- c("Nie", "Pon", "Wto", "Œro", "Czw", "Pi¹", "Sob")

ggseasonplot(confirmed1, year.labels = TRUE, year.labels.left = TRUE, season.labels = x_values) +
  ggtitle("Wykres sezonowy zaka¿eñ do I fali") + xlab("dzieñ tygodnia") + ylab("liczba zaka¿eñ")
ggseasonplot(confirmed1, polar = TRUE, main = "Wykres sezonowy zaka¿eñ do I fali", xlab = "") +
  labs(color = "tydzieñ")
ggsubseriesplot(confirmed1) +
  ggtitle("Podwykresy sezonowe zaka¿eñ do I fali") + xlab("dzieñ tygodnia") + ylab("liczba zaka¿eñ")
ggseasonplot(confirmed2, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Wykres sezonowy zaka¿eñ do II fali") + xlab("dzieñ tygodnia") + ylab("liczba zaka¿eñ")
ggseasonplot(confirmed2, polar = TRUE, main = "Wykres sezonowy zaka¿eñ do II fali", xlab = "") +
  labs(color = "tydzieñ")
ggsubseriesplot(confirmed2) +
  ggtitle("Podwykresy sezonowe zaka¿eñ do II fali") + xlab("dzieñ tygodnia") + ylab("liczba zaka¿eñ")
ggseasonplot(confirmed3, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Wykres sezonowy zaka¿eñ do III fali") + xlab("dzieñ tygodnia") + ylab("liczba zaka¿eñ")
ggseasonplot(confirmed3, polar = TRUE, main = "Wykres sezonowy zaka¿eñ do III fali", xlab = "") + 
  labs(color = "tydzieñ")
ggsubseriesplot(confirmed3) +
  ggtitle("Podwykresy sezonowe zaka¿eñ do III fali") + xlab("dzieñ tygodnia") + ylab("liczba zaka¿eñ")
ggseasonplot(confirmed4, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Wykres sezonowy zaka¿eñ do IV fali") + xlab("dzieñ tygodnia") + ylab("liczba zaka¿eñ")
ggseasonplot(confirmed4, polar = TRUE, main = "Wykres sezonowy zaka¿eñ do IV fali", xlab = "") +
  labs(color = "tydzieñ")
ggsubseriesplot(confirmed4) +
  ggtitle("Podwykresy sezonowe zaka¿eñ do IV fali") + xlab("dzieñ tygodnia") + ylab("liczba zaka¿eñ")

ggseasonplot(deaths1, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Wykres sezonowy œmierci do I fali") + xlab("dzieñ tygodnia") + ylab("liczba œmierci")
ggseasonplot(deaths1, polar = TRUE, main = "Wykres sezonowy œmierci do I fali", xlab = "") +
  labs(color = "tydzieñ")
ggsubseriesplot(deaths1) +
  ggtitle("Podwykresy sezonowe œmierci do I fali") + xlab("dzieñ tygodnia") + ylab("liczba œmierci")
ggseasonplot(deaths2, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Wykres sezonowy œmierci do II fali") + xlab("dzieñ tygodnia") + ylab("liczba œmierci")
ggseasonplot(deaths2, polar = TRUE, main = "Wykres sezonowy œmierci do II fali", xlab = "") +
  labs(color = "tydzieñ")
ggsubseriesplot(deaths2) +
  ggtitle("Podwykresy sezonowe œmierci do II fali") + xlab("dzieñ tygodnia") + ylab("liczba œmierci")
ggseasonplot(deaths3, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Wykres sezonowy œmierci do III fali") + xlab("dzieñ tygodnia") + ylab("liczba œmierci")
ggseasonplot(deaths3, polar = TRUE, main = "Wykres sezonowy œmierci do III fali", xlab = "") + 
  labs(color = "tydzieñ")
ggsubseriesplot(deaths3) +
  ggtitle("Podwykresy sezonowe œmierci do III fali") + xlab("dzieñ tygodnia") + ylab("liczba œmierci")
ggseasonplot(deaths4, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Wykres sezonowy œmierci do IV fali") + xlab("dzieñ tygodnia") + ylab("liczba œmierci")
ggseasonplot(deaths4, polar = TRUE, main = "Wykres sezonowy œmierci do IV fali", xlab = "") +
  labs(color = "tydzieñ")
ggsubseriesplot(deaths4) +
  ggtitle("Podwykresy sezonowe œmierci do IV fali") + xlab("dzieñ tygodnia") + ylab("liczba œmierci")

#wykresy rozrzutu
qplot(tests1, confirmed1, xlab = "liczba testóW", ylab = "liczba zaka¿eñ")
qplot(tests2, confirmed2, xlab = "liczba testóW", ylab = "liczba zaka¿eñ")
qplot(tests3, confirmed3, xlab = "liczba testóW", ylab = "liczba zaka¿eñ")
qplot(tests4, confirmed4, xlab = "liczba testóW", ylab = "liczba zaka¿eñ")

ggpairs(as.data.frame(cbind(tests7, confirmed7)))
ggpairs(as.data.frame(cbind(tests7, confirmed7)))
ggpairs(as.data.frame(cbind(tests7, confirmed7)))
ggpairs(as.data.frame(cbind(tests7, confirmed7)))

#wykresy opóŸnieñ
gglagplot(confirmed7)
gglagplot(confirmed7)
gglagplot(confirmed7)
gglagplot(confirmed7)

#ACF/PACF

for (i in 1:4) {
  tsdisplay(c_time_series[[i]], main = paste("Wykresy dla szeregu czasowego zaka¿eñ dla ", i, " fali"))
  tsdisplay(d_time_series[[i]], main = paste("Wykresy dla szeregu czasowego œmierci dla ", i, " fali"))
}

rm(x_labs, x_values)
          