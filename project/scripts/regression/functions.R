replace_na_values <- function(ts){
  ts_ind <- time(ts)[is.na(ts)] #157, 530
  for(i in 1:length(ts_ind)){
    ind <- calc_obs_num(calc_obs_week(ts_ind[i]))
    if((ind - 7) > 0)
      ts[ind] <- ts[ind - 7]
    else
      ts[ind] <- ts[ind - 1]
  }
  return(ts)
}
