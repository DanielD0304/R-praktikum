x = rgamma(30, shape= 3, rate = 2)
x_bar = mean(x)
s = var(x)
alpha_mm = x_bar^2/ s
beta_mm = x_bar / s

loglik_gamma = function(par, x) {
    alpha = par[1]
    beta = par[2]
    n = length(x)
    
    loglik = n * alpha * log(beta) - n * lgamma(alpha) + 
             (alpha - 1) * sum(log(x)) - beta * sum(x)
    return(loglik)
}
start = c(alpha_mm, beta_mm)

result = optim(start, loglik_gamma, x = x, control = list(fnscale = -1))
alpha_ml = result$par[1]
beta_ml = result$par[2]

plot(NULL, xlim = c(0,5), ylim = c(0,0.7), xlab= "x", ylab= "Dichte", main = "Titel", lwd = 2)
curve(dgamma(x,shape= 3, rate =2), add=TRUE, col = "red", lwd = 2)
curve(dgamma(x, shape= alpha_mm, rate = beta_mm), add=TRUE, col = "blue", lwd = 2)
curve(dgamma(x, shape= alpha_ml, rate= beta_ml), add=TRUE, col = "green", lwd = 2)

legend("topright", 
       legend = c("Wahre Γ(3,2)", "Momentenmethode", "Maximum-Likelihood"),
       col = c("red", "blue", "green"), lwd = 2)