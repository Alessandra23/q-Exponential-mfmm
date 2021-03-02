
# theme_set(theme_bw())

save_file <- "~/Documents/GitHub/Mestrado/q-Exponential-mfmm/Simulation/Results/"
setwd(save_file)


## plots to theta = 1/9

values <- list(N = 10000,
               n = 1000,
               mu = log(3),
               theta = 1/9)

filename <- paste('N', values$N,
                  'n', values$n,
                  'mu', round(values$mu,2),
                  'theta', round(values$theta,2),
                  sep='')
data_filename <-  paste(filename, '_data.RData',sep='')
load(paste(save_file, data_filename,sep=''))

samples <- data


# Graphics for standardized theta

#' theta = 1/9, n = 50, 91, 1000

values <- list(n = 100,
               mu = log(3),
               theta = 1/9)
n.values = c(50, 91, 1000)
v.values <- c(0.1,0.2,0.3,0.4)

comp.n.q.pad(values = values, n.values = n.values, v.values = v.values)


#' theta = 1, n = 153, 305, 5000

values <- list(N = 100,
               mu = log(3),
               theta = 1)
n.values = c(153, 305, 5000)
v.values <- c(0.1,0.2,0.3,0.4)

comp.n.q.pad(values = values, n.values = n.values, v.values = v.values)


#' theta = 9, n = 4179, 9987, 25000

values <- list(N = 100,
               mu = log(3),
               theta = 1)
n.values = c(4179, 9987, 25000)
v.values <- c(0.1,0.2,0.3,0.4)

comp.n.q.pad(values = values, n.values = n.values, v.values = v.values)



