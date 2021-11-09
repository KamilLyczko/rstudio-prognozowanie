#wykresy czasowe
plot(confirmed1, main = "Wykres czasowy zakażeń do I fali", xlab = "tydzień", ylab = "liczba zakażeń")
plot(confirmed2, main = "Wykres czasowy zakażeń do II fali", xlab = "tydzień", ylab = "liczba zakażeń")
plot(confirmed3, main = "Wykres czasowy zakażeń do III fali", xlab = "tydzień", ylab = "liczba zakażeń")
plot(confirmed4, main = "Wykres czasowy zakażeń do IV fali", xlab = "tydzień", ylab = "liczba zakażeń")

x_labs <- c(seq(data$date[1], data$date[length(data$date)], 30))
x_labs[length(x_labs) + 1] <- data$date[length(data$date)]

ggplot(data = data, aes(date, confirmed)) + 
  geom_line() +
  labs(title = "Wykres czasowy liczby zakażeń", x = "data", y = "liczba zakażeń") +
  scale_x_date(breaks = x_labs, labels = x_labs, date_labels = "%d-%m-%Y") +
  theme(axis.text.x = element_markdown(angle = 45, hjust = 1))

ggplot(data = data, aes(date, deaths)) + 
  geom_line() +
  labs(title = "Wykres czasowy liczby śmierci", x = "data", y = "liczba śmierci") +
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

plot(deaths1, main = "Wykres czasowy śmierci do I fali", xlab = "tydzień", ylab = "liczba śmierci")
plot(deaths2, main = "Wykres czasowy śmierci do II fali", xlab = "tydzień", ylab = "liczba śmierci")
plot(deaths3, main = "Wykres czasowy śmierci do III fali", xlab = "tydzień", ylab = "liczba śmierci")
plot(deaths4, main = "Wykres czasowy śmierci do IV fali", xlab = "tydzień", ylab = "liczba śmierci")

#wykresy sezonowe
x_values <- c("Nie", "Pon", "Wto", "Śro", "Czw", "Pią", "Sob")

ggseasonplot(confirmed1, year.labels = TRUE, year.labels.left = TRUE, season.labels = x_values) +
  ggtitle("Wykres sezonowy zakażeń do I fali") + xlab("dzień tygodnia") + ylab("liczba zakażeń")
ggseasonplot(confirmed1, polar = TRUE, main = "Wykres sezonowy zakażeń do I fali", xlab = "") +
  labs(color = "tydzień")
ggsubseriesplot(confirmed1) +
  ggtitle("Podwykresy sezonowe zakażeń do I fali") + xlab("dzień tygodnia") + ylab("liczba zakażeń")
ggseasonplot(confirmed2, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Wykres sezonowy zakażeń do II fali") + xlab("dzień tygodnia") + ylab("liczba zakażeń")
ggseasonplot(confirmed2, polar = TRUE, main = "Wykres sezonowy zakażeń do II fali", xlab = "") +
  labs(color = "tydzień")
ggsubseriesplot(confirmed2) +
  ggtitle("Podwykresy sezonowe zakażeń do II fali") + xlab("dzie? tygodnia") + ylab("liczba zakażeń")
ggseasonplot(confirmed3, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Wykres sezonowy zakażeń do III fali") + xlab("dzień tygodnia") + ylab("liczba zakażeń")
ggseasonplot(confirmed3, polar = TRUE, main = "Wykres sezonowy zakażeń do III fali", xlab = "") + 
  labs(color = "tydzień")
ggsubseriesplot(confirmed3) +
  ggtitle("Podwykresy sezonowe zakażeń do III fali") + xlab("dzień tygodnia") + ylab("liczba zakażeń")
ggseasonplot(confirmed4, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Wykres sezonowy zakażeń do IV fali") + xlab("dzie? tygodnia") + ylab("liczba zakażeń")
ggseasonplot(confirmed4, polar = TRUE, main = "Wykres sezonowy zakażeń do IV fali", xlab = "") +
  labs(color = "tydzień")
ggsubseriesplot(confirmed4) +
  ggtitle("Podwykresy sezonowe zakażeń do IV fali") + xlab("dzień tygodnia") + ylab("liczba zakażeń")

ggseasonplot(deaths1, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Wykres sezonowy śmierci do I fali") + xlab("dzień tygodnia") + ylab("liczba śmierci")
ggseasonplot(deaths1, polar = TRUE, main = "Wykres sezonowy śmierci do I fali", xlab = "") +
  labs(color = "tydzień")
ggsubseriesplot(deaths1) +
  ggtitle("Podwykresy sezonowe śmierci do I fali") + xlab("dzień tygodnia") + ylab("liczba śmierci")
ggseasonplot(deaths2, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Wykres sezonowy śmierci do II fali") + xlab("dzień tygodnia") + ylab("liczba śmierci")
ggseasonplot(deaths2, polar = TRUE, main = "Wykres sezonowy śmierci do II fali", xlab = "") +
  labs(color = "tydzień")
ggsubseriesplot(deaths2) +
  ggtitle("Podwykresy sezonowe śmierci do II fali") + xlab("dzień tygodnia") + ylab("liczba śmierci")
ggseasonplot(deaths3, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Wykres sezonowy śmierci do III fali") + xlab("dzień tygodnia") + ylab("liczba śmierci")
ggseasonplot(deaths3, polar = TRUE, main = "Wykres sezonowy śmierci do III fali", xlab = "") + 
  labs(color = "tydzień")
ggsubseriesplot(deaths3) +
  ggtitle("Podwykresy sezonowe śmierci do III fali") + xlab("dzień tygodnia") + ylab("liczba śmierci")
ggseasonplot(deaths4, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Wykres sezonowy śmierci do IV fali") + xlab("dzień tygodnia") + ylab("liczba śmierci")
ggseasonplot(deaths4, polar = TRUE, main = "Wykres sezonowy śmierci do IV fali", xlab = "") +
  labs(color = "tydzień")
ggsubseriesplot(deaths4) +
  ggtitle("Podwykresy sezonowe śmierci do IV fali") + xlab("dzień tygodnia") + ylab("liczba śmierci")

#wykresy rozrzutu
qplot(tests1, confirmed1, xlab = "liczba testóW", ylab = "liczba zakażeń")
qplot(tests2, confirmed2, xlab = "liczba testóW", ylab = "liczba zakażeń")
qplot(tests3, confirmed3, xlab = "liczba testóW", ylab = "liczba zakażeń")
qplot(tests4, confirmed4, xlab = "liczba testóW", ylab = "liczba zakażeń")

ggpairs(as.data.frame(cbind(tests7, confirmed7)))
ggpairs(as.data.frame(cbind(tests7, confirmed7)))
ggpairs(as.data.frame(cbind(tests7, confirmed7)))
ggpairs(as.data.frame(cbind(tests7, confirmed7)))

#wykresy opóźnień
gglagplot(confirmed7)
gglagplot(confirmed7)
gglagplot(confirmed7)
gglagplot(confirmed7)

#ACF/PACF

for (i in 1:4) {
  tsdisplay(c_time_series[[i]], main = paste("Wykresy dla szeregu czasowego zakażeń dla ", i, " fali"))
  tsdisplay(d_time_series[[i]], main = paste("Wykresy dla szeregu czasowego śmierci dla ", i, " fali"))
}

rm(x_labs, x_values, i)
          