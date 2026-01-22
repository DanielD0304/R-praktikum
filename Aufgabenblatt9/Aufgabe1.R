#a
X = matrix(c(1, 1, 1, 1,
              1, 2, 3, 4),
            nrow=4, ncol=2)

y = c(1.2, 1.3, 3.5, 3)

model_a = lm(y ~ X[,2])

beta_a = coef(model_a)
cat("(a) β (Koeffizienten):\n")
print(beta_a)

y_hat_a = fitted(model_a)
cat("\n(a) ŷ (Geschätzte Werte):\n")
print(y_hat_a)

epsilon_hat_a = residuals(model_a)
cat("\n(a) ε̂ (Residuen):\n")
print(epsilon_hat_a)

#b
X = matrix(c(1, 1, 1, 1,
              1.2, 1.3, 3.5, 3),
            nrow=4, ncol=2)

y = c(1, 2, 3, 4)

model_b = lm(y ~ X[,2])

beta_b = coef(model_b)
cat("\n(b) β (Koeffizienten):\n")
print(beta_b)

y_hat_b = fitted(model_b)
cat("\n(b) ŷ (Geschätzte Werte):\n")
print(y_hat_b)

epsilon_hat_b = residuals(model_b)
cat("\n(b) ε̂ (Residuen):\n")
print(epsilon_hat_b)

# Streudiagramm: (1.2, 1.3, 3.5, 3)^T gegen (1, 2, 3, 4)^T
x_data = c(1.2, 1.3, 3.5, 3)
y_data = c(1, 2, 3, 4)

plot(x_data, y_data, main="Streudiagramm mit Regressionsgeraden", 
     xlab="(1.2, 1.3, 3.5, 3)", ylab="(1, 2, 3, 4)", 
     pch=16, cex=1.5, xlim=c(0.8, 4), ylim=c(0, 4.5))

# Gerade aus (b): y = β0_b + β1_b * x
x_range = seq(0.8, 4, length.out=100)
y_line_b = beta_b[1] + beta_b[2] * x_range
lines(x_range, y_line_b, col="blue", lwd=2)

y_line_a = (x_range - beta_a[1]) / beta_a[2]
lines(x_range, y_line_a, col="red", lwd=2, lty=2)

x_intersect = (beta_a[2] * beta_b[1] + beta_a[1]) / (1 - beta_a[2] * beta_b[2])
y_intersect = beta_b[1] + beta_b[2] * x_intersect
points(x_intersect, y_intersect, pch=17, col="green", cex=2)

# (d)
for(i in 1:length(x_data)) {
  y_pred_b = beta_b[1] + beta_b[2] * x_data[i]
  segments(x_data[i], y_data[i], x_data[i], y_pred_b, col="blue", lty=3, lwd=1.5)
}

for(i in 1:length(x_data)) {
  y_pred_a = (x_data[i] - beta_a[1]) / beta_a[2]
  segments(x_data[i], y_data[i], x_data[i], y_pred_a, col="red", lty=3, lwd=1.5)
}

legend("topleft", 
       legend=c("Datenpunkte", "Gerade (b): y ~ x", "Gerade (a) transformiert: x ~ y", 
                "Schnittpunkt", "Abstände (a)", "Abstände (b)"), 
       pch=c(16, NA, NA, 17, NA, NA), lty=c(NA, 1, 2, NA, 3, 3), 
       col=c("black", "blue", "red", "green", "blue", "red"), lwd=c(NA, 2, 2, NA, 1.5, 1.5))

