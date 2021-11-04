#Prognozy metod naiwnych liczby œmierci dla I fali 
deaths1.train <- window(deaths1, end = weekly_freq_day_number(length(deaths1) - 30))
deaths1.test <- window(deaths1, start = weekly_freq_day_number(length(deaths1) - 29))

d1_naive_forecasts <- naive_forecasts(deaths1.train, 30)
plot_title1 <- "Prognozy metody naiwnej liczby œmierci dla I fali"
plot_title2 <- "Porównanie prognoz liczby œmierci z szeregiem testowym dla I fali"

generate_naive_forecasts_plot(deaths1.train, d1_naive_forecasts, plot_title1, "liczba œmierci")
generate_naive_test_comparison_plot(deaths1.test, d1_naive_forecasts, plot_title2, "liczba œmierci")

rm(deaths1.train, deaths1.test, d1_naive_forecasts, plot_title1, plot_title2)

#Prognozy metod naiwnych liczby œmierci dla II fali 
deaths2.train <- window(deaths2, end = weekly_freq_day_number(length(deaths2) - 30))
deaths2.test <- window(deaths2, start = weekly_freq_day_number(length(deaths2) - 29))

d2_naive_forecasts <- naive_forecasts(deaths2.train, 30)
plot_title1 <- "Prognozy metody naiwnej liczby œmierci dla II fali"
plot_title2 <- "Porównanie prognoz liczby œmierci z szeregiem testowym dla II fali"

generate_naive_forecasts_plot(deaths2.train, d2_naive_forecasts, plot_title1, "liczba œmierci")
generate_naive_test_comparison_plot(deaths2.test, d2_naive_forecasts, plot_title2, "liczba œmierci")

rm(deaths2.train, deaths2.test, d2_naive_forecasts, plot_title1, plot_title2)

#Prognozy metod naiwnych liczby œmierci dla III fali 
deaths3.train <- window(deaths3, end = weekly_freq_day_number(length(deaths3) - 30))
deaths3.test <- window(deaths3, start = weekly_freq_day_number(length(deaths3) - 29))

d3_naive_forecasts <- naive_forecasts(deaths3.train, 30)
plot_title1 <- "Prognozy metody naiwnej liczby œmierci dla III fali"
plot_title2 <- "Porównanie prognoz liczby œmierci z szeregiem testowym dla III fali"

generate_naive_forecasts_plot(deaths3.train, d3_naive_forecasts, plot_title1, "liczba œmierci")
generate_naive_test_comparison_plot(deaths3.test, d3_naive_forecasts, plot_title2, "liczba œmierci")

rm(deaths3.train, deaths3.test, d3_naive_forecasts, plot_title1, plot_title2)

#Prognozy metod naiwnych liczby œmierci dla IV fali 
deaths4.train <- window(deaths4, end = weekly_freq_day_number(length(deaths4) - 30))
deaths4.test <- window(deaths4, start = weekly_freq_day_number(length(deaths4) - 29))

d4_naive_forecasts <- naive_forecasts(deaths4.train, 30)
plot_title1 <- "Prognozy metody naiwnej liczby œmierci dla IV fali"
plot_title2 <- "Porównanie prognoz liczby œmierci z szeregiem testowym dla IV fali"

generate_naive_forecasts_plot(window(deaths4.train, start = weekly_freq_day_number(500)),
                              d4_naive_forecasts, plot_title1, "liczba œmierci")
generate_naive_test_comparison_plot(deaths4.test, d4_naive_forecasts, plot_title2, "liczba œmierci")

rm(deaths4.train, deaths4.test, d4_naive_forecasts, plot_title1, plot_title2)


