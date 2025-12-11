#' ---
#' title: "Blatt 4 Aufgabe 2"
#' author: "Daniel Daenecke"
#' date: "`r Sys.Date()`"
#' output: html_document
#' ---

farben = rainbow(6)

plot(NULL, xlim = c(-4, 4), ylim = c(0, 0.4), 
     xlab = "x", ylab = "Dichte", main = expression(Y[s] %->% N(0,1) ~ "für" ~ s %->% infinity))

#degrees of freedom
s_werte <- c(1, 2, 5, 10, 30, 100)


for(i in 1:length(s_werte)) {
    curve(dt(x, df = s_werte[i]), add = TRUE, col = farben[i], lwd = 2)
}
curve(dnorm(x), add = TRUE, col = "black", lwd = 2, lty = 2)


legend("topright", 
       legend = c(paste("t", s_werte, sep = "_"), "N(0,1)"),
       col = c(farben, "black"),
       lty = c(rep(1, length(s_werte)), 2),
       lwd = 2)

plot(NULL, xlim = c(-4,4), ylim = c(0, 0.5), xlab = "x", ylab = "Dichte", main = expression((X[s] - s) / sqrt(2*s) %->% N(0,1) ~ "für" ~ s %->% infinity))
for(i in 1:length(s_werte)) {
    s = s_werte[i]
    # Z = (X_s - s) / sqrt(2s)
    # X_s = Z*sqrt(2s) + s
    curve(dchisq(x*sqrt(2*s) + s, df = s) * sqrt(2*s), add = TRUE, col = farben[i], lwd = 2)
}
curve(dnorm(x), add = TRUE, col = "black", lwd = 2, lty = 2)


legend("topright", 
       legend = c(paste("s=", s_werte, sep = "_"), "N(0,1)"),
       col = c(farben, "black"),
       lty = c(rep(1, length(s_werte)), 2),
       lwd = 2)