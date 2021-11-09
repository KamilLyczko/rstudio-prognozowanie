c_scatterplots <- list()
d_scatterplots <- list()
for(i in 1:length(c_time_series)) {
  plot_title <- paste0("Wykres rozrzutu wartości obserwacji szeregów czasowych dla ",i, " fali")
  label_size <- 10
  title_size <- 11
  c_scatterplots[[i]] <- ggplot(data = data.frame(x = t_time_series[[i]], y = c_time_series[[i]]), aes(x, y)) +
    geom_point() +
    labs(title = plot_title, x = "liczba testów", y = "liczba zakażeń") +
    theme(axis.title.x = element_text(size = label_size), axis.title.y = element_text(size = label_size),
          plot.title = element_text(size = title_size))
  d_scatterplots[[i]] <- ggplot(data = data.frame(x = t_time_series[[i]], y = d_time_series[[i]]), aes(x, y)) +
    geom_point() +
    labs(title = plot_title, x = "liczba testów", y = "liczba śmierci") +
    theme(axis.title.x = element_text(size = label_size), axis.title.y = element_text(size = label_size),
          plot.title = element_text(size = title_size))
}
grid.arrange(grobs = c_scatterplots, ncol = 1)
grid.arrange(grobs = d_scatterplots, ncol = 1)

rm(c_scatterplots, d_scatterplots, i, plot_title, label_size, title_size)