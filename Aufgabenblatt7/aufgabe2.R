
library(PASWR2)
data(WASHER)
par(mfrow = c(2,2), mar = c(4, 4, 2, 2))
boxplot(WASHER$diameter)
hist(WASHER$diameter)
qqnorm(WASHER$diameter)
qqline(WASHER$diameter, col = "red")
print(shapiro.test(WASHER$diameter))
#b
# rechtsseitiger hypothesentest
# H0: varianz = 0.004
# H1: varianz != 0.004
#library(EnvStats)
#varTest(WASHER$diameter, sigma.squared = 0.004, alternative = "two.sided")

n <- length(WASHER$diameter)
s2 <- var(WASHER$diameter)
sigma0_sq <- 0.004

# Teststatistik
chi2_stat <- (n - 1) * s2 / sigma0_sq

# Kritischer Wert χ²_{n-1;1-α}
chi2_crit_95 <- qchisq(0.95, df = n - 1)

# p-Wert (rechtsseitiger Test)
p_value <- 1 - pchisq(chi2_stat, df = n - 1)

print(paste("Teststatistik χ²:", round(chi2_stat, 4)))
print(paste("Kritischer Wert χ²_{19;0.95}:", round(chi2_crit_95, 4)))
print(paste("p-Wert:", round(p_value, 4)))