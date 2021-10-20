decom_a <- decompose(data7$confirmed, type = "additive")
decom_m <- decompose(data7$confirmed, type = "multiplicative")
p_a <- autoplot(decom_a)
p_m <- autoplot(decom_m)
grid.arrange(grobs = list(p_a, p_m), ncol = 2)

decom_a1 <- decompose(confirmed7_1, type = "additive")
decom_m1 <- decompose(confirmed7_1, type = "multiplicative")
p_a1 <- autoplot(decom_a1)
p_m1 <- autoplot(decom_m1)
grid.arrange(grobs = list(p_a1, p_m1), ncol = 2)

decom_a2 <- decompose(confirmed7_2, type = "additive")
decom_m2 <- decompose(confirmed7_2, type = "multiplicative")
p_a2 <- autoplot(decom_a2)
p_m2 <- autoplot(decom_m2)
grid.arrange(grobs = list(p_a2, p_m2), ncol = 2)

decom_a3 <- decompose(confirmed7_3, type = "additive")
decom_m3 <- decompose(confirmed7_3, type = "multiplicative")
p_a3 <- autoplot(decom_a3)
p_m3 <- autoplot(decom_m3)
grid.arrange(grobs = list(p_a3, p_m3), ncol = 2)

