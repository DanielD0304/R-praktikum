library(gamair)
data(hubble)


plot(hubble$x, hubble$y, main="Hubble Datensatz", xlab="Distanz (x)", ylab="Geschwindigkeit (y)")

model = lm(y ~ x - 1, data=hubble)

beta_hat = coef(model)
cat("===== Modell 1: ALLE Daten =====\n")
cat("β (Hubble-Konstante):", beta_hat, "\n\n")

# Modell ohne Beobachtungen 3 und 15
hubble_filtered = hubble[-c(3, 15), ]
model_filtered = lm(y ~ x - 1, data=hubble_filtered)
beta_hat_filtered = coef(model_filtered)
cat("\n===== Modell 2: OHNE Beobachtungen 3 und 15 =====\n")
cat("β (Hubble-Konstante):", beta_hat_filtered, "\n\n")

# Beide Regressionsgeraden einzeichnen
abline(model, col="red", lwd=2.5, lty=1)
abline(model_filtered, col="green", lwd=2.5, lty=2)

points(hubble$x[c(3, 15)], hubble$y[c(3, 15)], col="orange", pch=8, cex=2)

legend("topleft", 
       legend=c("Datenpunkte", "Alle Daten (rot)", "Ohne 3, 15 (grün)", "Ausreißer 3, 15"), 
       pch=c(1, NA, NA, 8), lty=c(NA, 1, 2, NA), 
       col=c("black", "red", "green", "orange"), lwd=c(NA, 2.5, 2.5, NA))


identify(hubble$x, hubble$y, labels=rownames(hubble))

# ===== BERECHNUNG DES ALTERS DES UNIVERSUMS =====
cat("\n\n===== UNIVERSUMSALTER =====\n")

Mpc_in_km = 3.086e19  # 1 Megaparsec in km

H0_inverse = 1 / (beta_hat / Mpc_in_km)  # in Sekunden
H0_inverse_filtered = 1 / (beta_hat_filtered / Mpc_in_km)  # in Sekunden

# In Jahre umrechnen (1 Jahr = 365.25 Tage × 24 h × 3600 s)
seconds_per_year = 365.25 * 24 * 3600

age_universe_model1 = H0_inverse / seconds_per_year
age_universe_model2 = H0_inverse_filtered / seconds_per_year

cat("\n--- Modell 1: Alle Daten ---\n")
cat("β (H0) =", beta_hat, "km/s/Mpc\n")
cat("Alter des Universums ≈", round(age_universe_model1, 2), "Jahre\n")
cat("                  ≈", round(age_universe_model1 / 1e9, 2), "Milliarden Jahre\n")

cat("\n--- Modell 2: Ohne Beobachtungen 3 und 15 ---\n")
cat("β (H0) =", beta_hat_filtered, "km/s/Mpc\n")
cat("Alter des Universums ≈", round(age_universe_model2, 2), "Jahre\n")
cat("                  ≈", round(age_universe_model2 / 1e9, 2), "Milliarden Jahre\n")

cat("\n--- VERGLEICH ---\n")
cat("Differenz:", round(abs(age_universe_model1 - age_universe_model2) / 1e9, 2), "Milliarden Jahre\n")