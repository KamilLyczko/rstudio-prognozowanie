log_confirmed <- log(confirmed)
log_confirmed[is.infinite(log_confirmed)] <- 0
log_deaths <- log(deaths)
log_deaths[is.infinite(log_deaths)] <- 0
decom_c <- stl(log_confirmed, s.window = 7)
decom_d <- stl(log_deaths, s.window = 7)
conf_m_ts <- cbind(confirmed,
                   exp(decom_c$time.series[, 2]),
                   exp(decom_c$time.series[, 1]),
                   exp(decom_c$time.series[, 3]))
deaths_m_ts <- cbind(deaths,
                   exp(decom_d$time.series[, 2]),
                   exp(decom_d$time.series[, 1]),
                   exp(decom_d$time.series[, 3]))
col_names <- c("data", "trend", "seasonal", "remainder")
colnames(conf_m_ts) <- col_names
colnames(deaths_m_ts) <- col_names
plot.ts(conf_m_ts, main = "Dekompozycja szeregu czasowego liczby zakażeń", xlab = "tydzień")
plot.ts(deaths_m_ts, main = "Dekompozycja szeregu czasowego liczby śmierci", xlab = "tydzień")
rm(log_confirmed, log_deaths, decom_c, decom_d, conf_m_ts, deaths_m_ts, col_names )


