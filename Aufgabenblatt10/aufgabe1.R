intervalls = 1:15
bakterien = c(355, 211, 197, 166, 142, 106, 104, 60, 56, 38, 36, 32, 21, 19, 15)
data = c(intervalls, bakterien)

log_bakterien = log(bakterien)

par(mfrow = c(1, 2))
plot(intervalls, bakterien, main = "Originaldaten", 
     xlab = "Bestrahlungsintervall", ylab = "Anzahl Bakterien")
plot(intervalls, log_bakterien, main = "Log-transformierte Daten",
     xlab = "Bestrahlungsintervall", ylab = "log(Anzahl Bakterien)")

modell_log = lm(log_bakterien ~ intervalls)

abline(modell_log, col = "red", lwd = 2)

konfidenz = confint(modell_log, level = 0.975)

par(mfrow = c(1, 1))

ellipse_points = ellipse(modell_log, level = 0.95)
plot(ellipse_points, col = "red", lwd = 2, type = "l")

points(modell_log$coefficients[1], modell_log$coefficients[2], pch = 19, cex = 1.5, col = "black")

x1 = konfidenz[1,1]
x2 = konfidenz[1,2]
y1 = konfidenz[2,1]
y2 = konfidenz[2,2]
segments(x0 = c(x1,x1), x1 = c(x1,x2), y0 = c(y1,y1), y1 = c(y2,y1))
segments(x0 = c(x2,x2), x1 = c(x1,x2), y0 = c(y2,y2), y1 = c(y2,y1))

anfangsbestand = exp(coef(modell_log)[1])

bakt = c(anfangsbestand, bakterien)
beta1_hat = coef(modell_log)[2]
multi_faktor = exp(beta1_hat)
abnahme = (1 - multi_faktor) * 100

plot(intervalls, bakterien, 
     main = "Exponentielles Zerfallsmodell (rücktransformiert)",
     xlab = "Bestrahlungsintervall", 
     ylab = "Anzahl Bakterien",
     pch = 19, cex = 1.2, col = "blue")

t_werte = seq(1, 15, length.out = 100)
N_angepasst = exp(coef(modell_log)[1] + coef(modell_log)[2] * t_werte)

lines(t_werte, N_angepasst, col = "red", lwd = 3)
