library(devtools)
devtools::install_github("Alessandra23/q-Exponential-mfmm/qExponential")
library(qExponential)

save_file = "~/Documents/GitHub/Mestrado/q-Exponential-mfmm/Simulation/Results/"

N <- 10
n <- 5
mu <- log(3)
theta <- c(1/9, 1, 9, 100)

allcomb <- expand.grid(N = N,
                    n = n,
                    mu = mu,
                    theta = theta)


ncomb <- nrow(allcomb)
seed  <- 4

for (i in 1:ncomb) {
  set.seed(seed)
  comb <- allcomb[i,]
  N <- comb$N
  n <- comb$n
  mu <- comb$mu
  theta <- comb$theta

  data <- qexp.samples(N = N,n = n, theta = theta, mu = mu)

  filename <- paste('N', N,
                    'n', n,
                    'mu', round(mu,2),
                    'theta', round(theta,2),
                    sep='')

  data_filename <- paste(filename, '_data.RData', sep='')
  save(data, file = paste(save_file, data_filename, sep=''))

}
