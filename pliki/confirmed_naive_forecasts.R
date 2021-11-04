#Prognozy metod naiwnych liczby zaka¿eñ dla I fali 
confirmed1.train <- window(confirmed1, end = weekly_freq_day_number(length(confirmed1) - 30))
confirmed1.test <- window(confirmed1, start = weekly_freq_day_number(length(confirmed1) - 29))

c1_naive_forecasts <- naive_forecasts(confirmed1.train, 30)
plot_title1 <- "Prognozy metody naiwnej liczby zaka¿eñ dla I fali"
plot_title2 <- "Porównanie prognoz liczby zaka¿eñ z szeregiem testowym dla I fali"

generate_naive_forecasts_plot(confirmed1.train, c1_naive_forecasts, plot_title1, "liczba zaka¿eñ")
generate_naive_test_comparison_plot(confirmed1.test, c1_naive_forecasts, plot_title2, "liczba zaka¿eñ")

rm(confirmed1.train, confirmed1.test, c1_naive_forecasts, plot_title1, plot_title2)

#Prognozy metod naiwnych liczby zaka¿eñ dla II fali 
confirmed2.train <- window(confirmed2, end = weekly_freq_day_number(length(confirmed2) - 30))
confirmed2.test <- window(confirmed2, start = weekly_freq_day_number(length(confirmed2) - 29))

c2_naive_forecasts <- naive_forecasts(confirmed2.train, 30)
plot_title1 <- "Prognozy metody naiwnej liczby zaka¿eñ dla II fali"
plot_title2 <- "Porównanie prognoz liczby zaka¿eñ z szeregiem testowym dla II fali"

generate_naive_forecasts_plot(confirmed2.train, c2_naive_forecasts, plot_title1, "liczba zaka¿eñ")
generate_naive_test_comparison_plot(confirmed2.test, c2_naive_forecasts, plot_title2, "liczba zaka¿eñ")

rm(confirmed2.train, confirmed2.test, c2_naive_forecasts, plot_title1, plot_title2)

#Prognozy metod naiwnych liczby zaka¿eñ dla III fali 
confirmed3.train <- window(confirmed3, end = weekly_freq_day_number(length(confirmed3) - 30))
confirmed3.test <- window(confirmed3, start = weekly_freq_day_number(length(confirmed3) - 29))

c3_naive_forecasts <- naive_forecasts(confirmed3.train, 30)
plot_title1 <- "Prognozy metody naiwnej liczby zaka¿eñ dla III fali"
plot_title2 <- "Porównanie prognoz liczby zaka¿eñ z szeregiem testowym dla III fali"

generate_naive_forecasts_plot(confirmed3.train, c3_naive_forecasts, plot_title1, "liczba zaka¿eñ")
generate_naive_test_comparison_plot(confirmed3.test, c3_naive_forecasts, plot_title2, "liczba zaka¿eñ")

rm(confirmed3.train, confirmed3.test, c3_naive_forecasts, plot_title1, plot_title2)

#Prognozy metod naiwnych liczby zaka¿eñ dla IV fali 
confirmed4.train <- window(confirmed4, end = weekly_freq_day_number(length(confirmed4) - 30))
confirmed4.test <- window(confirmed4, start = weekly_freq_day_number(length(confirmed4) - 29))

c4_naive_forecasts <- naive_forecasts(confirmed4.train, 30)
plot_title1 <- "Prognozy metody naiwnej liczby zaka¿eñ dla IV fali"
plot_title2 <- "Porównanie prognoz liczby zaka¿eñ z szeregiem testowym dla IV fali"

generate_naive_forecasts_plot(window(confirmed4.train, start = weekly_freq_day_number(500)), 
                              c4_naive_forecasts, plot_title1, "liczba zaka¿eñ")
generate_naive_test_comparison_plot(confirmed4.test, c4_naive_forecasts, plot_title2, "liczba zaka¿eñ")

rm(confirmed4.train, confirmed4.test, c4_naive_forecasts, plot_title1, plot_title2)


