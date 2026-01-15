#a
par(mfrow = c(2,2))
p_werte = c()
for(i in 1:10000) {
    data = rnorm(20)
    test = t.test(data, mu = 0, alternative = "two.sided")
    p_werte = c(p_werte, test$p.value)
}
hist(p_werte)

#b
# mu = -0.5
p_werte = c()
for(i in 1:10000) {
    data = rnorm(20, mean = -0.5)
    test = t.test(data, mu = 0, alternative = "two.sided")
    p_werte = c(p_werte, test$p.value)
}
hist(p_werte, main = "Hist of mu = -0.5")
# mu = 1
p_werte = c()
for(i in 1:10000) {
    data = rnorm(20, mean = 1)
    test = t.test(data, mu = 0, alternative = "two.sided")
    p_werte = c(p_werte, test$p.value)
}
hist(p_werte, main = "Hist of mu = 1")
# mu = 0.5
p_werte = c()
for(i in 1:10000) {
    data = rnorm(20, mean = 0.5)
    test = t.test(data, mu = 0, alternative = "two.sided")
    p_werte = c(p_werte, test$p.value)
}
hist(p_werte, main = "Hist of mu = 0.5")