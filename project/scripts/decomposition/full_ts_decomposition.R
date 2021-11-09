decom_c_a <- decompose(confirmed, type = "additive")
decom_c_m <- decompose(confirmed, type = "multiplicative")
decom_d_a <- decompose(deaths, type = "additive")
decom_d_m <- decompose(deaths, type = "multiplicative")

text_size <- 10
title_c <- "Dekompozycja szeregu czasowego liczby zakażeń"
c_plot_a <- autoplot(decom_c_a) +
  labs(title = paste(title_c, " - model addytywny")) + xlab("numer tygodnia") +
  theme(plot.title = element_text(size = text_size))
c_plot_m <- autoplot(decom_c_m) +
  labs(title = paste(title_c, " - model multiplikatywny")) + xlab("numer tygodnia") +
  theme(plot.title = element_text(size = text_size))
title_d <- "Dekompozycja szeregu czasowego liczby śmierci"
d_plot_a <- autoplot(decom_d_a) +
  labs(title = paste(title_d, " - model addytywny")) + xlab("numer tygodnia") +
  theme(plot.title = element_text(size = text_size))
d_plot_m <- autoplot(decom_d_m) +
  labs(title = paste(title_d, " - model multiplikatywny")) + xlab("numer tygodnia") +
  theme(plot.title = element_text(size = text_size))

plots_c <- list(c_plot_a, c_plot_m)
plots_d <- list(d_plot_a, d_plot_m)
grid.arrange(grobs = plots_c)
grid.arrange(grobs = plots_d)


rm(c_plot_a, d_plot_a, c_plot_m, d_plot_m, decom_c_a, decom_c_m, decom_d_a, decom_d_m, title_c, title_d, plots_c, plots_d, text_size)
