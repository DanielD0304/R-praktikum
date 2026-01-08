obs = 1:5
freq = c(63, 14, 5, 1, 2)
xvec = rep(obs, freq)
table = table(xvec)
rel.freq = table(xvec) /sum(freq)
barplot(rel.freq, main="", xlab="relative Haeufigkeit")
loglikelihoodfunction = function(p, x) {
    -(p+1)*sum(log(x))- length(x)*log(VGAM::zeta(p+1))
}
p_schaetzer =  optimize(loglikelihoodfunction, interval = c(0.01, 10), x =xvec , maximum = TRUE)

p_hat = p_schaetzer$maximum
k = 1:5

prob_angepasst = k^(-(p_hat + 1)) / VGAM::zeta(p_hat + 1)

daten = rbind(rel.freq, prob_angepasst)

barplot(daten, beside = TRUE, 
        names.arg = 1:5,
        col = c("blue", "red"),
        xlab = "Beobachtung", 
        ylab = "Häufigkeit/Wahrscheinlichkeit",
        legend = c("Relative Häufigkeit", "Angepasste Wahrscheinlichkeit"))