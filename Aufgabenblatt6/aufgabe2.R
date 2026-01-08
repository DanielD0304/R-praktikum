guetefunktion = function(mu, var, n, mu_0) {
    sd_mean = sqrt(n) / var
    1 - pt(qt(0.95, n - 1), n - 1, sd_mean *(mu - mu_0))
}

n = c(10, 40)
var = c(2, 1)
theta = seq(0, 10, length.out = 200)
guete_a = sapply(theta, function(t) guetefunktion(mu = t, var = var[1], n = n[1], mu_0 = 5))
guete_b = sapply(theta, function(t) guetefunktion(mu = t, var = var[1], n = n[2], mu_0 = 5))
guete_c = sapply(theta, function(t) guetefunktion(mu = t, var = var[2], n = n[1], mu_0 = 5))
guete_d = sapply(theta, function(t) guetefunktion(mu = t, var = var[2], n = n[2], mu_0 = 5))

plot(theta, guete_a, type = "l", col = "blue", lwd = 2,
    xlab = expression(theta), ylab = "Gütefunktion",
    main = "Gütefunktionen der Tests für verschiedene Stichprobenumfänge",
    ylim = c(0, 1))
lines(theta, guete_b, col = "red", lwd = 2)
lines(theta, guete_c, col = "yellow", lwd = 2)
lines(theta, guete_d, col = "pink", lwd = 2)
abline(h = 0.05, lty = 2, col = "darkgreen")
legend("topright", legend = c("n = 10, var = 2", "n = 40, var = 1", expression(alpha == 0.05)),
      col = c("blue", "red", "darkgreen"), lwd = c(2, 2, 1), lty = c(1, 1, 2), bty = "n")