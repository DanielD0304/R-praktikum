intervalle = matrix(NA, nrow = 100, ncol = 2)
for(i in 1:100) {
    random = rnorm(5, 0, 1)
    rmean = mean(random)
    rsd = sd(random)
    t = qt(0.975, 4)
    coeff = rsd/sqrt(5)
    upper = rmean + t*coeff
    lower = rmean - t*coeff
    intervalle[i, ] = c(lower, upper)
}
has_mu = (intervalle[,1] <= 0) & (intervalle[,2] >= 0)
anzahl = sum(has_mu)
cat("Anteil:", anzahl / 100 * 100, "%\n")

intervall_plot = function(intervall_matrix) {
    lowerbounds = intervall_matrix[,1]
    upperbounds = intervall_matrix[,2]
    has_mu = (lowerbounds <= 0) & (upperbounds >= 0)
    anzahl = sum(has_mu)
    cat("Anteil:", anzahl / 100 * 100, "%\n")
    plot(NULL, ylim = c(-5,5), xlim = c(0,100), type = "n")
    ci = ifelse((lowerbounds <= 0) & (upperbounds >= 0), "black", "red") 
    segments(1:100, y0 = lowerbounds, 1:100, y1 = upperbounds, col = ci)
}
intervall_plot(intervalle)
