train <- window(confirmed2, end = weekly_freq_day_number(length(confirmed2) - 30))
add1_train <- train + 1 #zwiększenie wartości obserwacji o 1 w celi eliminacji wartości 0
                        #wymagane ze względu na przeprowadzane transformacje
p1 <- list()
p2 <- list()
p3 <- list()
main_title <- 10
axis_titles <- 10
p1[[1]] <- p2[[1]] <- p3[[1]] <- generate_ts_time_plot(add1_train, 
                                 "Wykres czasowy liczb zakażeń zwiększonych o 1 dla 2 fali", "liczba zakażeń")
                                 + set_titles_size(main_title, axis_titles)
log_add1_train <- log(add1_train)
p2[[2]] <- generate_ts_time_plot(log_add1_train, 
                      "Wykres czasowy zwiększonych o 1 i zlogarytmowanych liczb zakażeń dla 2 fali", 
                      "liczba zakażeń") + set_titles_size(main_title, axis_titles)
lambda <- BoxCox.lambda(add1_train)
boxcox_add1_train <- BoxCox(add1_train, lambda)
p3[[2]] <- generate_ts_time_plot(boxcox_add1_train, 
                      paste0("Wykres czasowy liczb zakażeń dla 2 fali po transformacji Box-Cox (lambda = ",
                      signif(lambda, 3), ")"), 
                      "liczba zakażeń") + set_titles_size(main_title, axis_titles)
diff_list <- make_stationary_ts(add1_train)
p1[[2]] <- generate_ts_time_plot(diff_list$diff_ts, 
                      paste0("Wykres czasowy zwiększonego o 1 szeregu liczb zakażeń dla 2 fali ",
                             "po różnicowaniach (sezonowe: ",
                             diff_list$diffs[1], ", pierwsze: ", diff_list$diffs[2], ")")) +
                             set_titles_size(main_title, axis_titles)
diff_list_log <- make_stationary_ts(log_add1_train)
p2[[3]] <- generate_ts_time_plot(diff_list_log$diff_ts, 
                      paste0("Wykres czasowy zwiększonego o 1 szeregu liczb zakażeń dla 2 fali po ", 
                             "logarytmowaniu i różnicowaniach (sezonowe: ",
                             diff_list_log$diffs[1], ", pierwsze: ", diff_list_log$diffs[2], ")")) +
                             set_titles_size(main_title, axis_titles)
diff_list_boxcox <- make_stationary_ts(boxcox_add1_train)
p3[[3]] <- generate_ts_time_plot(diff_list_boxcox$diff_ts, 
                      paste0("Wykres czasowy szeregu liczby zakażeń dla 2 fali po ", 
                             "transformacji Box-Cox i różnicowaniach (sezonowe: ",
                             diff_list_boxcox$diffs[1], ", pierwsze: ", diff_list_boxcox$diffs[2], ")")) +
                             set_titles_size(main_title, axis_titles)
grid.arrange(grobs = p1, ncol = 1)
grid.arrange(grobs = p2, ncol = 1)
grid.arrange(grobs = p3, ncol = 1)
#Najlepszy efekt (wyrównanie wariancji) uzyskano dla transformacji Box-Cox

rm(train, add1_train, p1, p2, p3, main_title, axis_titles)
rm(log_add1_train, boxcox_add1_train, lambda)
rm(diff_list, diff_list_log, diff_list_boxcox)