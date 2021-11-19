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


