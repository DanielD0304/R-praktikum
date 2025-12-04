B = 20000
results = data.frame()

# Spaltennamen definieren
col_names <- c(
    "n20_N_mean", "n20_N_median", "n20_t3_mean", "n20_t3_median", "n20_t5_mean", "n20_t5_median",
    "n40_N_mean", "n40_N_median", "n40_t3_mean", "n40_t3_median", "n40_t5_mean", "n40_t5_median",
    "n80_N_mean", "n80_N_median", "n80_t3_mean", "n80_t3_median", "n80_t5_mean", "n80_t5_median",
    "n160_N_mean", "n160_N_median", "n160_t3_mean", "n160_t3_median", "n160_t5_mean", "n160_t5_median"
)

sample= 20
for(b in 1:B) {          # ÄUSSERE Schleife: Wiederholungen
    row = numeric(24)    # Platz für 24 Werte (4 Stichprobengrößen * 3 Verteilungen * 2 Kennzahlen)
    col_index = 1
    for(i in 0:3) {       # INNERE Schleife: Stichprobengrößen
        #sample size doubles each iteration
        sample_size = sample * 2^i
        #sample from N(0,1)
        sample_n = rnorm(sample_size, mean = 0, sd = 1)
        row[col_index:(col_index+1)] = c(mean(sample_n), median(sample_n))
        #print Mean and Median
        #sample from t-3-distribution
        sample_t3 = rt(sample_size, df = 3) / sqrt(3 / (3-2))
        row[(col_index+2):(col_index+3)] = c(mean(sample_t3), median(sample_t3))
        #sample from t-5-distribution
        sample_t5 = rt(sample_size, df = 5) / sqrt(5 / (5-2))
        row[(col_index+4):(col_index+5)] = c(mean(sample_t5), median(sample_t5))
        col_index = col_index + 6
    }
    results = rbind(results, row) # Append the row to results
}

# Spaltennamen zuweisen
colnames(results) <- col_names

variances = apply(results, 2, var)
print(variances)
quotient = numeric(12)  # Vektor mit 12 Plätzen erstellen
for(i in seq(2, 24, by=2)) {
    quotient[i/2] = variances[i] / variances[i - 1]
}
print(quotient)
