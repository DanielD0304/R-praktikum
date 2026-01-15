#aufgabe a
p = numeric(5)
for(i in 1:5){
    p[i] = pnorm(3 + i*2, mean=3, sd=4) - pnorm(3 - i*2, mean=3, sd=4)
}
print(p)

#aufgabe b
par(mfrow=c(1,1))
curve(dnorm(x, mean=3, sd=4), from = -10, to = 16, main="Dichtefunktion N(3,4)", ylab="Dichte")
# Fläche einfärben mit vielen vertikalen Linien (type="h")
x_vals <- seq(-10, 16, length=1000)
y_vals <- dnorm(x_vals, mean=3, sd=4)
lines(x_vals, y_vals, type="h", col="lightblue")

# Kurve nochmal schwarz drüber zeichnen
curve(dnorm(x, mean=3, sd=4), add=TRUE, lwd=2)

for(i in 1:3){
    lines(x = 3  - i*2, y = 0.1, col=i, type = "h", lwd=2)
    lines(x = 3  + i*2, y = 0.1, col=i, type = "h", lwd=2)
}