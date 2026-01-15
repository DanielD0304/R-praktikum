#Einseitiger Hypothesentest
#linksseitiger Test mit n = 10
#b
p = binom.test(0, 10, p = 0.2, alternative = "less")
#c
interval = binom.test(0,10,p = 0.2, conf.level = 0.99)
