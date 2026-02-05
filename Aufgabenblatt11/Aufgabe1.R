
setwd("C:/Users/danie/Projects/Raufgaben/Aufgabenblatt11")
wine_data <- read.table("wine.txt", header=TRUE, sep="")

model = lm(price ~ temp + h.rain + w.rain, data = wine_data)

fitted_values = fitted(model)

new_model = lm(price ~ temp + h.rain + w.rain + year, data = wine_data)
model_r = summary(model)$r.squared
model_sigma = summary(model)$sigma
model_new_r = summary(new_model)$r.squared
model_new_sigma = summary(new_model)$sigma

log_model <- lm(log(price) ~ temp + h.rain + w.rain + year, data = wine_data)
print(coef(log_model))

par(mfrow = c(2, 2))
plot(new_model)
plot(fitted(new_model), residuals(new_model),
     main = "Modell (2): Residuen vs. Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")

qqnorm(residuals(new_model), main = "Modell (2): Normal Q-Q Plot")
qqline(residuals(new_model), col = "red")

plot(log_model)
plot(fitted(log_model), residuals(log_model),
     main = "Modell (3): Residuen vs. Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")


log_model4 <- lm(log(price) ~ temp + h.rain + w.rain, data = wine_data)

anova_linear <- anova(model, new_model)

anova_log <- anova(log_model4, log_model)

alpha <- 0.01
p_value_linear <- anova_linear$`Pr(>F)`[2]
p_value_log <- anova_log$`Pr(>F)`[2]

print(paste("Lineares Modell - p-Wert:", round(p_value_linear, 6)))
print(paste("Log-Modell - p-Wert:", round(p_value_log, 6)))

mean_temp <- mean(wine_data$temp)
mean_h_rain <- mean(wine_data$h.rain)
mean_w_rain <- mean(wine_data$w.rain)

data_1985 <- data.frame(
  year = 1985,
  temp = mean_temp,
  h.rain = mean_h_rain,
  w.rain = mean_w_rain
)

prediction_1985 <- predict(log_model, newdata = data_1985, 
                          interval = "prediction", level = 0.99)

price_prediction <- exp(prediction_1985)

print(paste("Geschätzter Preis:", round(price_prediction[1], 2)))