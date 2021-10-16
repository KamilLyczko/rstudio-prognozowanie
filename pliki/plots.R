#wykres czasowy
plot(covid_poland_ts365, main = "Wykresy czasowe", ylab = "a", xlab = "czas")
plot(covid_poland_ts365[, "confirmed"], main = "Wykres zaka¿eñ", xlab = "czas", ylab = "liczba zaka¿eñ")
plot(covid_poland_ts365[, "deaths"], main = "Wykres œmierci", xlab = "czas", ylab = "liczba œmierci")
plot(covid_poland_ts365[, "tests"], main = "Wykres testóW", xlab = "czas", ylab = "liczba testów")
autoplot(covid_poland_ts365[, "confirmed"], colour = "red") + ggtitle("Wykres zaka¿eñ") + xlab("czas") + ylab("liczba zaka¿eñ")
ggseasonplot(covid_poland_ts7[, "confirmed"])
ggseasonplot(covid_poland_ts7[, "confirmed"], polar = TRUE)

#wykresy czasowe
plot(confirmed7_1, main = "Wykres czasowy zachorowañ do I fali", xlab = "tydzieñ", ylab = "liczba zachorowañ")
plot(confirmed7_2, main = "Wykres czasowy zachorowañ do II fali", xlab = "tydzieñ", ylab = "liczba zachorowañ")
plot(confirmed7_3, main = "Wykres czasowy zachorowañ do III fali", xlab = "tydzieñ", ylab = "liczba zachorowañ")
plot(confirmed7_4, main = "Wykres czasowy zachorowañ do IV fali", xlab = "tydzieñ", ylab = "liczba zachorowañ")

plot(deaths7_1, main = "Wykres czasowy œmierci do I fali", xlab = "tydzieñ", ylab = "liczba œmierci")
plot(deaths7_2, main = "Wykres czasowy œmierci do II fali", xlab = "tydzieñ", ylab = "liczba œmierci")
plot(deaths7_3, main = "Wykres czasowy œmierci do III fali", xlab = "tydzieñ", ylab = "liczba œmierci")
plot(deaths7_4, main = "Wykres czasowy œmierci do IV fali", xlab = "tydzieñ", ylab = "liczba œmierci")

#wykresy sezonowe
ggseasonplot(confirmed7_1, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Wykres sezonowy zachorowañ do I fali") + xlab("dzieñ tygodnia") + ylab("liczba zachorowañ")
ggseasonplot(confirmed7_1, polar = TRUE, main = "Wykres sezonowy zachorowañ do I fali", xlab = "")
ggsubseriesplot(confirmed7_1) +
  ggtitle("Podwykresy sezonowe zachorowañ do I fali") + xlab("dzieñ tygodnia") + ylab("liczba zachorowañ")
ggseasonplot(confirmed7_2, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Wykres sezonowy zachorowañ do II fali") + xlab("dzieñ tygodnia") + ylab("liczba zachorowañ")
ggseasonplot(confirmed7_2, polar = TRUE, main = "Wykres sezonowy zachorowañ do II fali", xlab = "")
ggsubseriesplot(confirmed7_2) +
  ggtitle("Podwykresy sezonowe zachorowañ do II fali") + xlab("dzieñ tygodnia") + ylab("liczba zachorowañ")
ggseasonplot(confirmed7_3, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Wykres sezonowy zachorowañ do III fali") + xlab("dzieñ tygodnia") + ylab("liczba zachorowañ")
ggseasonplot(confirmed7_3, polar = TRUE, main = "Wykres sezonowy zachorowañ do III fali", xlab = "")
ggsubseriesplot(confirmed7_3) +
  ggtitle("Podwykresy sezonowe zachorowañ do III fali") + xlab("dzieñ tygodnia") + ylab("liczba zachorowañ")
ggseasonplot(confirmed7_4, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Wykres sezonowy zachorowañ do IV fali") + xlab("dzieñ tygodnia") + ylab("liczba zachorowañ")
ggseasonplot(confirmed7_4, polar = TRUE, main = "Wykres sezonowy zachorowañ do IV fali", xlab = "")
ggsubseriesplot(confirmed7_4) +
  ggtitle("Podwykresy sezonowe zachorowañ do IV fali") + xlab("dzieñ tygodnia") + ylab("liczba zachorowañ")

ggseasonplot(deaths7_1, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Wykres sezonowy œmierci do I fali") + xlab("dzieñ tygodnia") + ylab("liczba œmierci")
ggseasonplot(deaths7_1, polar = TRUE, main = "Wykres sezonowy œmierci do I fali", xlab = "")
ggsubseriesplot(deaths7_1) +
  ggtitle("Podwykresy sezonowe œmierci do I fali") + xlab("dzieñ tygodnia") + ylab("liczba œmierci")
ggseasonplot(deaths7_2, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Wykres sezonowy œmierci do II fali") + xlab("dzieñ tygodnia") + ylab("liczba œmierci")
ggseasonplot(deaths7_2, polar = TRUE, main = "Wykres sezonowy œmierci do II fali", xlab = "")
ggsubseriesplot(deaths7_2) +
  ggtitle("Podwykresy sezonowe œmierci do II fali") + xlab("dzieñ tygodnia") + ylab("liczba œmierci")
ggseasonplot(deaths7_3, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Wykres sezonowy œmierci do III fali") + xlab("dzieñ tygodnia") + ylab("liczba œmierci")
ggseasonplot(deaths7_3, polar = TRUE, main = "Wykres sezonowy œmierci do III fali", xlab = "")
ggsubseriesplot(deaths7_3) +
  ggtitle("Podwykresy sezonowe œmierci do III fali") + xlab("dzieñ tygodnia") + ylab("liczba œmierci")
ggseasonplot(deaths7_4, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Wykres sezonowy œmierci do IV fali") + xlab("dzieñ tygodnia") + ylab("liczba œmierci")
ggseasonplot(deaths7_4, polar = TRUE, main = "Wykres sezonowy œmierci do IV fali", xlab = "")
ggsubseriesplot(deaths7_4) +
  ggtitle("Podwykresy sezonowe œmierci do IV fali") + xlab("dzieñ tygodnia") + ylab("liczba œmierci")

#wykresy rozrzutu
qplot(tests7_1, confirmed7_1, xlab = "liczba testóW", ylab = "liczba zachorowañ")
qplot(tests7_2, confirmed7_2, xlab = "liczba testóW", ylab = "liczba zachorowañ")
qplot(tests7_3, confirmed7_3, xlab = "liczba testóW", ylab = "liczba zachorowañ")
qplot(tests7_4, confirmed7_4, xlab = "liczba testóW", ylab = "liczba zachorowañ")

ggpairs(as.data.frame(cbind(tests7_1, confirmed7_1)))
ggpairs(as.data.frame(cbind(tests7_2, confirmed7_2)))
ggpairs(as.data.frame(cbind(tests7_3, confirmed7_3)))
ggpairs(as.data.frame(cbind(tests7_4, confirmed7_4)))

#wykresy opóŸnieñ
gglagplot(confirmed7_1)
gglagplot(confirmed7_2)
gglagplot(confirmed7_3)
gglagplot(confirmed7_4)

#ACF
ggAcf(confirmed7_1)
ggAcf(confirmed7_2)
ggAcf(confirmed7_3)
ggAcf(confirmed7_4)

ggAcf(deaths7_1)
ggAcf(deaths7_2)
ggAcf(deaths7_3)
ggAcf(deaths7_4)

#PACF
ggPacf(confirmed7_1)
ggPacf(confirmed7_2)
ggPacf(confirmed7_3)
ggPacf(confirmed7_4)

ggPacf(deaths7_1)
ggPacf(deaths7_2)
ggPacf(deaths7_3)
ggPacf(deaths7_4)
          