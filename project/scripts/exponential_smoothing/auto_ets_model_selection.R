c_models <- list()
d_models <- list()
for(i in 1:length(c_time_series)) {
  c_train <- window(c_time_series[[i]], end = weekly_freq_day_number(length(c_time_series[[i]] - 30)))
  d_train <- window(d_time_series[[i]], end = weekly_freq_day_number(length(d_time_series[[i]] - 30)))
  c_models[[i]] <- ets(c_train)
  d_models[[i]] <- ets(d_train)
  cat("Model dla szeregu liczby zakażeń dla ", i, "fali:\n")
  show(summary(c_models[[i]]))
  show(calculate_training_errors(c_models[[i]]))
  cat("Model dla szeregu liczby śmierci dla ", i, "fali:\n")
  show(summary(d_models[[i]]))
  show(calculate_training_errors(d_models[[i]]))
}

rm(c_models, d_models, c_train, d_train, i)