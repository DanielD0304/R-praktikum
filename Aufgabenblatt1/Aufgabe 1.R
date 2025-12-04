snowcover = c(25.6, 23.2, 25.0, 23.9, 25.1, 24.4, 21.2, 26.1, 23.2, 25.5, 
24.9, 24.0, 21.3, 23.8, 26.1, 26.0, 26.1, 25.1, 22.2, 23.4, 
22.6, 24.6, 25.0, 22.9, 22.1, 23.7, 24.4, 22.3)
snow = data.frame( year = 1996:2023, snowcover = snowcover)
par(mfrow = c(2,2))
plot(snow$year, snow$snowcover,type = "l",  col = "blue", main = "Snowcover over Years", xlab = "Year", ylab = "Snowcover")
lm.snow <- lm(snowcover ~ year, data = snow)
abline(lm.snow, col = "red", lwd = 2)
hist(snow$snowcover, main = "Histogram of Snowcover", xlab = "Snowcover", col = "lightblue")

logsnow <- log(snow$snowcover)
hist(logsnow, main = "Histogram of Log Snowcover", xlab = "Log Snowcover", col = "lightblue")
plot(snow$year, logsnow,type = "l",  col = "blue")
abline(lm(logsnow ~ year, data = snow), col = "red", lwd = 2)
write.csv2(snow, file = "C:/Users/danie/Projects/Raufgaben/snow.csv", row.names = FALSE)
snow2 = read.csv2("C:/Users/danie/Projects/Raufgaben/snow.csv")