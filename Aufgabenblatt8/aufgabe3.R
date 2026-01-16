library(lancor)
library(mvtnorm)
par(mfrow = (c(2,2)))
rho = c(0, 0.2, 0.4)
data_list = list()
for(i in 1:3) {
    data_list[[i]] = rmvnorm(10000, mean = c(0,0), sigma = matrix(c(1,rho[i],rho[i],1), nrow=2))
    plot(data_list[[i]], main = paste("Korrelation ρ =", rho[i]))
}
cor_1_2 = cor(data_list[[1]][,1], data_list[[1]][,2])
cor(data_list[[2]][,1], data_list[[2]][,2])
cor(data_list[[3]][,1], data_list[[3]][,2])
