par(mfrow=c(2,1))
x = seq(-1, 1, length.out=100)
y1 = exp(x)
plot(x,y1, type="l", col="blue", lwd=2,
     xlab="x", ylab="exp(x)", main="Plot der Exponentialfunktion")

y2 = x^2/2 + x+1
lines(x,y2, type="l", col="green", lwd=2)

curve(exp(x), from = -1, to = 1, col = "blue", lwd = 2)
curve((x^2/2) + x + 1, from = -1, to = 1, col = "green", lwd = 2, add = TRUE)