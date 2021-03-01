library(devtools)
devtools::install_github("Alessandra23/q-Exponential-mfmm/qExponential")
library(qExponential)

# theme_set(theme_bw())



# theta = 1/9 

values <- list(n = 5,
               mu = log(3),
               theta = 1/9,
               v = 0.4)

plot.reject(n = values$n, mu = values$mu, theta = values$theta, v = values$v)



# theta = 1

values <- list(n = 5,
               mu = log(3),
               theta = 1,
               v = 0.4)

plot.reject(n = values$n, mu = values$mu, theta = values$theta, v = values$v)


# theta = 9

values <- list(n = 5,
               mu = log(3),
               theta = 9,
               v = 0.4)

plot.reject(n = values$n, mu = values$mu, theta = values$theta, v = values$v)


