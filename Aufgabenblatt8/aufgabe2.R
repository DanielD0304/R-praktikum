
n = 20
set.seed(1)
random = rpois(n, 5)

lambda_0 = 5
lambda_hat = mean(random)

log_likelihood_poisson = function(lambda, data) {
  n = length(data)
  sum_xi_log_lambda = sum(data) * log(lambda)
  n_lambda = n * lambda
  sum_log_factorial = sum(log(factorial(data)))
  
  return(sum_xi_log_lambda - n_lambda - sum_log_factorial)
}

log_L_H0 = log_likelihood_poisson(lambda_0, random)
log_L_H1 = log_likelihood_poisson(lambda_hat, random)


LR_statistic = 2 * (log_L_H1 - log_L_H0)

alpha = 0.05
cv = qchisq(1 - alpha, df = 1)
