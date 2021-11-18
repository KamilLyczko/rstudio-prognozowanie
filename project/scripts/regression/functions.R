find_best_first_obs_numb <- function(begin_list, MAPE_matrix) {
  best <- data.frame(
    first_wave = c(obs_num = 1, MAPE = 1),
    second_wave = c(obs_num = 1, MAPE = 1),
    third_wave = c(obs_num = 1, MAPE = 1),
    fourth_wave = c(obs_num = 1, MAPE = 1)
  )
  for(i in 1:4) {
    min <- min(MAPE_matrix[, i])
    for(j in 1:length(MAPE_matrix[, i]))
      if(MAPE_matrix[j, i] == min)
        break
    best[1, i] <- begin_list[[j]][i]
    best[2, i] <- min
  }
  return(best)
}

#funkcja zwracająca indeks modelu w podanej liście,
#który charakteryzuje się najmniejszymi wartościami mierników błędów treningowych
find_best_fitting_model <- function(models_list) {
  training_errors_measures <- data.frame()
  for(i in 1:length(models_list)) {
    training_errors_measures <- rbind(training_errors_measures, calculate_training_errors(models_list[[i]]))
  }
  model_points <- c(rep(0, length(models_list)))
  for(i in 2:length(training_errors_measures)) {
    min <- min(training_errors_measures[, i])
    for(j in 1:length(training_errors_measures[, i])) {
      if(min == training_errors_measures[j, i])
        model_points[j] <- model_points[j] + 1
    }
  }
  return(match(max(model_points), model_points))
}

#funkcja zwracająca indeks modelu w podanej liście,
#który charakteryzuje się najmniejszymi wartościami mierników błędów testowych
find_best_forecast_model <- function(models_list, test_ts) {
  h <- length(test_ts)
  test_errors_measures <- data.frame()
  for(i in 1:length(models_list)) {
    predictions <- forecast(models_list[[i]], h = h)
    test_errors_measures <- rbind(test_errors_measures, calculate_ex_post_errors(predictions, test_ts))
  }
  model_points <- c(rep(0, length(models_list)))
  for(i in 2:length(test_errors_measures)) {
    min <- min(test_errors_measures[, i])
    for(j in 1:length(test_errors_measures[, i])) {
      if(min == test_errors_measures[j, i])
        model_points[j] <- model_points[j] + 1
    }
  }
  return(match(max(model_points), model_points))
}
