train <- window(confirmed2, end = weekly_freq_day_number(length(confirmed2) - 30))
add1_train <- train + 1
lambda <- BoxCox.lambda(add1_train)
boxcox_add1_train <- BoxCox(add1_train, lambda)
diff_list <- make_stationary_ts(boxcox_add1_train)
main_title <- 10
axis_titles <- 10
time_plots <- list()
cor_plots <- list()
time_plots[[1]] <- generate_ts_time_plot(add1_train, 
                                    "Wykres czasowy liczb zakażeń zwiększonych o 1 dla 2 fali", 
                                    "liczba zakażeń + 1") +
  set_titles_size(main_title, axis_titles)
time_plots[[2]] <- generate_ts_time_plot(boxcox_add1_train, 
                      paste0("Wykres czasowy liczb zakażeń dla 2 fali po transformacji Box-Cox (lambda = ",
                             signif(lambda, 3), ")"), 
                      "BoxCox(liczba zakażeń)") +
  set_titles_size(main_title, axis_titles)
time_plots[[3]] <- generate_ts_time_plot(diff_list$diff_ts, 
                                 paste0("Wykres czasowy szeregu liczby zakażeń dla 2 fali po ", 
                                        "transformacji Box-Cox i różnicowaniach (sezonowe: ",
                                        diff_list$diffs[1], ", pierwsze: ", diff_list$diffs[2], ")")) +
                                              set_titles_size(main_title, axis_titles)
cor_plots[[1]] <- ggAcf(diff_list$diff_ts) +
  ggtitle(paste0("Wykres autokorelacji transformowanego szeregu liczb zakażeń dla 2 fali po różnicowaniach ",
                 "s: ", diff_list$diffs[1], ", p: ", diff_list$diffs[2], ")")) +
  set_titles_size(main_title, axis_titles)
cor_plots[[2]] <- ggPacf(diff_list$diff_ts) +
  ggtitle(paste0("Wykres autokorelacji częściowej transformowanego szeregu liczb zakażeń dla 2 fali", 
                 "po różnicowaniach (s: ", diff_list$diffs[1], ", p: ", diff_list$diffs[2], ")")) +
  set_titles_size(main_title, axis_titles)
grid.arrange(grobs = time_plots, ncol = 1)
grid.arrange(grobs = cor_plots, ncol = 1)
#Na podstawie ACF: ARIMA(0,1,11)(0,0,2)[7]
#Na podstawie PACF: ARIMA(10,1,0)(1,0,0)[7]
orders_cand <- list(c(0,1,11,0,0,2), c(0,1,11,0,0,1), c(0,1,11,1,0,1), c(10,1,0,1,0,0),
                    c(10,1,0,2,0,0), c(9,1,0,1,0,0), c(8,1,0,1,0,0), c(10,1,1,1,0,0))
best_models <- find_best_arima_models(add1_train, orders_cand, lambda)
cat("Najlepsze modele:\n")
#najlepszy model z listy (wg aicc):
summary(best_models$best_model_aicc)
#najlepszy model z listy (wg błędów treningowych): 
summary(best_models$best_model_training_errors)
#najlepszy model wybrany automatycznie:
summary(best_models$best_model_auto)

checkresiduals(best_models$best_model_aicc) #reszty to biały szum
checkresiduals(best_models$best_model_training_errors) #reszty to biały szum
checkresiduals(best_models$best_model_auto)

#poszukiwanie modelu dającego najlepsze prognozy
test <- window(confirmed2, start = weekly_freq_day_number(length(confirmed2) - 29))
add1_test <- test + 1
best_model <- best_models[[find_best_forecast_model(best_models, test)]]
summary(best_model)
show(calculate_ex_post_errors(forecast(best_models[[1]], h = 30), test))
show(calculate_ex_post_errors(forecast(best_models[[2]], h = 30), test))
show(calculate_ex_post_errors(forecast(best_models[[3]], h = 30), test))

#prognozowanie z wykorzystaniem najlepszego modelu
forecast <- forecast(best_model, h = 30)
plots <- list(
  generate_fit_plot(best_model,
                    paste0("Wykres dopasowania modelu ", 
                           get_arima_model_type(best_model),
                           " do zmodyfikowanego szeregu liczb zakażeń dla 2 fali"),
                    "liczba zakażeń + 1"),
  generate_forecast_plot(forecast,
                         "Wykres prognoz liczb zakażeń dla 2 fali",
                         "liczba zakażeń + 1"),
  generate_test_comparison_plot(add1_test, forecast,
                                "Porównanie prognoz liczb zakażeń z wartościami rzeczywistymi",
                                "liczba zakażeń + 1")
)
adjusted_forecast <- make_adjusted_forecast_object(forecast, 1)
plots2 <- list(
  generate_forecast_plot2(adjusted_forecast,
                         "Wykres prognoz liczb zakażeń dla 2 fali",
                         "liczba zakażeń"),
  generate_test_comparison_plot(test, adjusted_forecast,
                                "Porównanie prognoz liczb zakażeń z wartościami rzeczywistymi",
                                "liczba zakażeń")
)
grid.arrange(grobs = plots, ncol = 1)
grid.arrange(grobs = plots2, ncol = 1)
show(calculate_ex_post_errors(adjusted_forecast, test))

rm(train, add1_train, lambda, boxcox_add1_train, diff_list)
rm(time_plots, cor_plots, main_title, axis_titles)
rm(orders_cand, best_models, best_model, test, add1_test, forecast, plots)
rm(adjusted_forecast, plots2)

#Najlepsze wyniki w oparciu o model ARIMA(1,1,2)(1,0,1)[7]
#Błędy testowe:
#            ME      MAE      MSE    RMSE     MAPE
#value 994.0067 3347.101 17285224 4157.55 15.68223