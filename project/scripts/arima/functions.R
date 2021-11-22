#funkcja zwracająca z podanej listy model z najmniejszą wartością aicc
find_model_best_aicc <- function(fit_list) {
  aiccs <- c()
  for(i in 1:length(fit_list)) {
    aiccs[i] <- fit_list[[i]]$aicc
  }
  return(fit_list[[match(min(aiccs), aiccs)]])
}