data = read.table("Aufgabenblatt12/yields.txt", header = TRUE)

yields = as.vector(t(cbind(data$sand, data$clay, data$loam)))

soil_type <- factor(rep(c("sand", "clay", "loam"), 10))

obs_number <- rep(1:10, each = 3)

plot(obs_number, yields, 
     col = c("brown", "red", "green")[soil_type], 
     pch = c(16, 17, 18)[soil_type],
     xlab = "Beobachtungsnummer", 
     ylab = "Ertrag",
     main = "Ertrag nach Beobachtungsnummer und Bodenart")

legend("topright", 
       legend = c("clay", "loam", "sand"), 
       col = c("brown", "red", "green"), 
       pch = c(16, 17, 18),
       title = "Bodenart")

boxplot(yields ~ soil_type, 
        col = c("red", "green", "brown"),
        xlab = "Bodenart", 
        ylab = "Ertrag",
        main = "Boxplots der Erträge nach Bodenarten")

overall_mean = mean(c(data$sand, data$clay, data$loam))
overall_sd = sd(c(data$sand, data$clay, data$loam))

means = sapply(data, mean)
sds = sapply(data, sd)

anova_result <- aov(yields ~ soil_type)
print(summary(anova_result))

n = 30
k = 3
n_i = 10
