material_A = c(14.0, 8.8, 11.2, 14.2, 11.8, 6.4, 9.8, 11.3, 9.3, 13.6)
material_B = c(13.2, 8.2, 10.9, 14.3, 10.7, 6.6, 9.5, 10.8, 8.8, 13.3)

t_test_result = t.test(material_A, material_B)
print(t_test_result)


par(mfrow = c(2, 3))

hist(material_A, probability = TRUE, ylim = c(0, 0.12))
mu_A = mean(material_A)
sd_A = sd(material_A)
curve(dnorm(x, mean = mu_A, sd = sd_A), add = TRUE, col = "red", lwd = 2)

hist(material_B, probability = TRUE, ylim = c(0, 0.12))
mu_B = mean(material_B)
sd_B = sd(material_B)
curve(dnorm(x, mean = mu_B, sd = sd_B), add = TRUE, col = "red", lwd = 2)

boxplot(material_A, material_B, names = c("Material A", "Material B"),
        main = "Boxplot-Vergleich", ylab = "Messwert", col = c("lightblue", "lightgreen"))

#c

corr = cor(material_A, material_B)
print(corr)
plot(material_A, material_B)
abline(lm(material_B ~ material_A), col = "red", lwd = 2)
print(t.test(material_A, material_B, paired = TRUE))


print(t.test(material_A, material_B))