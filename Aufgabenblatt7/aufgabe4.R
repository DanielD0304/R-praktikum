#a
mistake = 0
for (i in 1:10000) {
    data = rnorm(20)
    test = t.test(data, mu = 0, alternative = "two.sided")
    p = test$p.value
    if (p < 0.05) {
        mistake = mistake +1
    }
}
print(paste("Fehler 1. Art (alpha = 0.05):", mistake / 10000))
#b
# (i) Y := 2X - 1 mit X ~ Bin(1, 1/2)
mistake_b1 = 0
for (i in 1:10000) {
    X = rbinom(20, 1, 0.5)
    Y = 2 * X - 1
    test = t.test(Y, mu = 0, alternative = "two.sided")
    if (test$p.value < 0.05) {
        mistake_b1 = mistake_b1 + 1
    }
}
print(paste("(i) Y = 2X - 1, X ~ Bin(1, 1/2): Fehler 1. Art =", mistake_b1 / 10000))

# (ii) Y := X - 1 mit X ~ Exp(1)
mistake_b2 = 0
for (i in 1:10000) {
    X = rexp(20, 1)
    Y = X - 1
    test = t.test(Y, mu = 0, alternative = "two.sided")
    if (test$p.value < 0.05) {
        mistake_b2 = mistake_b2 + 1
    }
}
print(paste("(ii) Y = X - 1, X ~ Exp(1): Fehler 1. Art =", mistake_b2 / 10000))

# (iii) Y ~ t2
mistake_b3 = 0
for (i in 1:10000) {
    Y = rt(20, 2)
    test = t.test(Y, mu = 0, alternative = "two.sided")
    if (test$p.value < 0.05) {
        mistake_b3 = mistake_b3 + 1
    }
}
print(paste("(iii) Y ~ t2: Fehler 1. Art =", mistake_b3 / 10000))
#c
#a
n = 200
mistake = 0
for (i in 1:10000) {
    data = rnorm(n)
    test = t.test(data, mu = 0, alternative = "two.sided")
    p = test$p.value
    if (p < 0.05) {
        mistake = mistake +1
    }
}
print(paste("Fehler 1. Art (alpha = 0.05):", mistake / 10000))
#b
# (i) Y := 2X - 1 mit X ~ Bin(1, 1/2)
mistake_b1 = 0
for (i in 1:10000) {
    X = rbinom(n, 1, 0.5)
    Y = 2 * X - 1
    test = t.test(Y, mu = 0, alternative = "two.sided")
    if (test$p.value < 0.05) {
        mistake_b1 = mistake_b1 + 1
    }
}
print(paste("(i) Y = 2X - 1, X ~ Bin(1, 1/2): Fehler 1. Art =", mistake_b1 / 10000))

# (ii) Y := X - 1 mit X ~ Exp(1)
mistake_b2 = 0
for (i in 1:10000) {
    X = rexp(n, 1)
    Y = X - 1
    test = t.test(Y, mu = 0, alternative = "two.sided")
    if (test$p.value < 0.05) {
        mistake_b2 = mistake_b2 + 1
    }
}
print(paste("(ii) Y = X - 1, X ~ Exp(1): Fehler 1. Art =", mistake_b2 / 10000))

# (iii) Y ~ t2
mistake_b3 = 0
for (i in 1:10000) {
    Y = rt(n, 2)
    test = t.test(Y, mu = 0, alternative = "two.sided")
    if (test$p.value < 0.05) {
        mistake_b3 = mistake_b3 + 1
    }
}
print(paste("(iii) Y ~ t2: Fehler 1. Art =", mistake_b3 / 10000))