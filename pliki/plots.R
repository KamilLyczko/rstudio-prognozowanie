#wykres czasowy
plot(covid_poland_ts365, main = "Wykresy czasowe", ylab = "a", xlab = "czas")
plot(covid_poland_ts365[, "confirmed"], main = "Wykres zaka¿eñ", xlab = "czas", ylab = "liczba zaka¿eñ")
plot(covid_poland_ts365[, "deaths"], main = "Wykres œmierci", xlab = "czas", ylab = "liczba œmierci")
plot(covid_poland_ts365[, "tests"], main = "Wykres testóW", xlab = "czas", ylab = "liczba testów")
autoplot(covid_poland_ts365[, "confirmed"], colour = "red") + ggtitle("Wykres zaka¿eñ") + xlab("czas") + ylab("liczba zaka¿eñ")
ggseasonplot(covid_poland_ts7[, "confirmed"])
