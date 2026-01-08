
prob_error1_calculator = function(mean, var, n, diff) {
    sd_mean = sqrt(var) / sqrt(n)
    error1_prob = pnorm(40 - diff, mean = mean, sd = sd_mean) +
     pnorm(40 + diff, mean = mean, sd = sd_mean, lower.tail = FALSE)
    return(error1_prob)
}

prob_a = prob_error1_calculator(mean = 40, var = 36, n = 9, diff = 4)
prob_b = prob_error1_calculator(mean = 40, var = 36, n = 36, diff = 2)

guetefunktion = function(mean, var, n, diff) {
    prob_error1_calculator(mean, var, n ,diff)
}

theta = seq(30, 50, length.out = 200)

# (c) Plot der Gutefunktionen für beide Tests
guete_a = sapply(theta, function(t) guetefunktion(mean = t, var = 36, n = 9, diff = 4))
guete_b = sapply(theta, function(t) guetefunktion(mean = t, var = 36, n = 36, diff = 2))

plot(theta, guete_a, type = "l", col = "blue", lwd = 2,
    xlab = expression(theta), ylab = "Gütefunktion",
    main = "Gütefunktionen der Tests für verschiedene Stichprobenumfänge",
    ylim = c(0, 1))
lines(theta, guete_b, col = "red", lwd = 2)
abline(h = 0.05, lty = 2, col = "darkgreen")
legend("topright", legend = c("n = 9, diff = 4", "n = 36, diff = 2", expression(alpha == 0.05)),
      col = c("blue", "red", "darkgreen"), lwd = c(2, 2, 1), lty = c(1, 1, 2), bty = "n")