text_size <- 7

for (i in 1:4) {
  decom_c_a <- decompose(c_time_series[[i]], type = "additive")
  decom_c_m <- decompose(c_time_series[[i]], type = "multiplicative")
  title_c <- "Dekompozycja szeregu czasowego zaka¿eñ dla "
  c_plot_a <- autoplot(decom_c_a) +
    labs(title = paste(title_c, i, " fali - model addytywny")) +
    theme(plot.title = element_text(size = text_size))
  c_plot_m <- autoplot(decom_c_m) +
    labs(title = paste(title_c, i, " fali - model multiplikatywny")) +
    theme(plot.title = element_text(size = text_size))
  
  decom_d_a <- decompose(d_time_series[[i]], type = "additive")
  decom_d_m <- decompose(d_time_series[[i]], type = "multiplicative")
  title_d <- "Dekompozycja szeregu czasowego œmierci dla "
  d_plot_a <- autoplot(decom_d_a) +
    labs(title = paste(title_d, i, " fali - model addytywny")) +
    theme(plot.title = element_text(size = text_size))
  d_plot_m <- autoplot(decom_d_m) +
    labs(title = paste(title_d, i, " fali - model multiplikatywny")) +
    theme(plot.title = element_text(size = text_size))
  
  plots <- list(c_plot_a, c_plot_m, d_plot_a, d_plot_m)
  grid.arrange(grobs = plots, ncol = 2)
}

rm(c_plot_a, d_plot_a, c_plot_m, d_plot_m, decom_c_a, decom_c_m, title_c, decom_d_a, decom_d_m, title_d, plots, i, text_size)