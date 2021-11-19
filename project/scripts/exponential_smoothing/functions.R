#funkcja zwracająca łańcuch tekstowy opisujący model
#stosowany przez wykorzystaną metodę wygładzania wykładniczego
get_methods_ets_model <- function(forecast_object) {
  model <- forecast_object$model$components
  if(model[4] == 'TRUE')
    model[2] <- paste0(model[2], 'd')
  return(paste0("ETS(", model[1], model[2], model[3], ")"))
}