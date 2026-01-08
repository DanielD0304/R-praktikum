n = 20
k = 9
p = 0.25
alpha = 0.05

wald_interval = function(k, n, alpha = 0.05) {
  p_hat = k / n
  z = qnorm(1 - alpha/2)
  se = sqrt(p_hat * (1 - p_hat) / n)
  lower = p_hat - z * se
  upper = p_hat + z * se
  c(lower, upper)
}

clopper_pearson = function(k, n, alpha = 0.05) {
  lower <- if (k == 0) 0 else qbeta(alpha/2, k, n - k + 1)
  upper <- if (k == n) 1 else qbeta(1 - alpha/2, k + 1, n - k)
  c(lower, upper)
}