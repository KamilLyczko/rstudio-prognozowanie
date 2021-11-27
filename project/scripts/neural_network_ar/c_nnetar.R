#dla każdego szeregu zostaną automatycznie dopasowane 2 modele: nnar bez regresora zewnętrznego
#oraz z regresorem zewnętrznym (liczba wykonanych testów)
#wyłoniony zostanie lepszy z nich na podstawie jakości generowanych prognoz na okres 30 dni

#wartości startowe wag sieci są losowe dla każdego wywołania funkcji nnetar
#dlatego porównanie modeli z oraz bez dodatkowego regresora zostanie wykonane po 100 razy


best_models <- list()
for(i in 1:length(c_time_series)) {
  c_train <- window(c_time_series[[i]], 
                    end = weekly_freq_day_number(length(c_time_series[[i]]) - 30))
  t_train <- window(t_time_series_cleaned[[i]], 
                    end = weekly_freq_day_number(length(t_time_series_cleaned[[i]]) - 30))
  c_test <- window(c_time_series[[i]],
                   start = weekly_freq_day_number(length(c_time_series[[i]]) - 29))
  t_test <- window(t_time_series_cleaned[[i]],
                   start = weekly_freq_day_number(length(t_time_series_cleaned[[i]]) - 29))
  model_no_reg <- 0
  model_reg <- 0
  iter <- 100
  for(j in 1:iter) {
    cat(i,j, "\n")
    auto_model <- nnetar(c_train)
    auto_model_reg <- nnetar(c_train, xreg = t_train)
    forecast1 <- forecast(auto_model, h = 30)
    forecast2 <- forecast(auto_model_reg, xreg = t_test)
    ex_post_errors1 <- calculate_ex_post_errors(forecast1, c_test)
    ex_post_errors2 <- calculate_ex_post_errors(forecast2, c_test)
    model1_points <- length(which((ex_post_errors1[, 2:5] < ex_post_errors2[, 2:5]) == TRUE))
    model2_points <- length(which((ex_post_errors1[, 2:5] > ex_post_errors2[, 2:5]) == TRUE))
    if(model1_points < model2_points)
      model_reg <- model_reg + 1
    else
      model_no_reg <- model_no_reg + 1
    cat("model_no_reg: ", model_no_reg, "\n")
    cat("model_reg: ", model_reg, "\n")
    if((model_no_reg > iter/2)|(model_reg > iter/2))
      break
  }
  if(model_reg > model_no_reg)
    best_models[[i]] <- nnetar(c_train, xreg = t_train)
  else
    best_models[[i]] <- nnetar(c_train)
  cat(i,j, "\n")
}

mes <- "Najlepsze modele:\n"
for(i in 1:length(best_models)) {
  mes <- paste0(mes, i, " fala: ", 
                get_nnar_model_type(best_models[[i]]),
                "\n")
}
cat(mes)

#prognozowanie
for(i in 1:length(best_models)) {
  c_test <- window(c_time_series[[i]],
                   start = weekly_freq_day_number(length(c_time_series[[i]]) - 29))
  predictions <- NULL
  if(is.null(best_models[[i]]$xreg)) {
    predictions <- forecast(best_models[[i]], h = 30)
  }
  else {
    t_test <- window(t_time_series_cleaned[[i]],
                     start = weekly_freq_day_number(length(t_time_series_cleaned[[i]]) - 29))
    predictions <- forecast(best_models[[i]],
                            xreg = t_test)
  }
  plots <- list(
    generate_fit_plot(best_models[[i]],
                      paste0("Wykres dopasowania modelu ", 
                             get_nnar_model_type(best_models[[i]]),
                             " do szeregu liczb zakażeń dla ", i, " fali"),
                      "liczba zakażeń") + set_titles_size(),
    generate_forecast_plot2(predictions,
                            paste0("Wykres prognoz liczb zakażeń dla ", i, " fali"),
                            "liczba zakażeń") + set_titles_size(),
    generate_test_comparison_plot(c_test, predictions,
                                  "Porównanie prognoz liczb zakażeń z wartościami rzeczywistymi",
                                  "liczba zakażeń") + set_titles_size()
  )
  grid.arrange(grobs = plots, ncol = 1)
}

rm(best_models, c_train, t_train, c_test, t_test, model_no_reg, model_reg, iter)
rm(auto_model, auto_model_reg, forecast1, forecast2, i, j)
rm(ex_post_errors1, ex_post_errors2, model1_points, model2_points, mes, predictions, plots)
