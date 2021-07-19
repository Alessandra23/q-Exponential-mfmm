library(devtools)
devtools::install_github("Alessandra23/q-Exponential-mfmm/qExponential")
library(qExponential)

library(gridExtra)
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


grid.arrange(analz.samples(values, samples = samples, v = -0.1)[[1]],
             analz.samples(values, samples = samples, v = 0.1)[[1]], ncol=2)

grid.arrange(analz.samples(values, samples = samples, v = -0.2)[[1]],
             analz.samples(values, samples = samples, v = 0.2)[[1]], ncol=2)

grid.arrange(analz.samples(values, samples = samples, v = -0.3)[[1]],
             analz.samples(values, samples = samples, v = 0.3)[[1]], ncol=2)

grid.arrange(analz.samples(values, samples = samples, v = -0.4)[[1]],
             analz.samples(values, samples = samples, v = 0.4)[[1]], ncol=2)

grid.arrange(analz.samples(values, samples = samples, v = -0.5)[[1]],
             analz.samples(values, samples = samples, v = 0.5)[[1]], ncol=2)


# All

grid.arrange(analz.samples(values, samples = samples, v = -0.1)[[1]],
             analz.samples(values, samples = samples, v = 0.1)[[1]], 
             analz.samples(values, samples = samples, v = -0.2)[[1]],
             analz.samples(values, samples = samples, v = 0.2)[[1]], 
             analz.samples(values, samples = samples, v = -0.3)[[1]],
             analz.samples(values, samples = samples, v = 0.3)[[1]], 
             analz.samples(values, samples = samples, v = -0.4)[[1]],
             analz.samples(values, samples = samples, v = 0.4)[[1]], 
             ncol=4)

# observing only the positive values of v
grid.arrange(analz.samples(values, samples = samples, v = 0.1)[[1]], 
             analz.samples(values, samples = samples, v = 0.2)[[1]], 
             analz.samples(values, samples = samples, v = 0.3)[[1]], 
             analz.samples(values, samples = samples, v = 0.4)[[1]], 
             ncol=4)

ana.samples.paper.1 <- analz.samples(values, samples = samples, v = 0.4)[[1]] + labs(title = "")

## plots to theta = 1

values <- list(N = 10000,
               n = 1000,
               mu = log(3),
               theta = 1)

filename <- paste('N', values$N,
                  'n', values$n,
                  'mu', round(values$mu,2),
                  'theta', round(values$theta,2),
                  sep='')
data_filename <-  paste(filename, '_data.RData',sep='')
load(paste(save_file, data_filename,sep=''))


samples <- data

grid.arrange(analz.samples(values, samples = samples, v = -0.1)[[1]],
             analz.samples(values, samples = samples, v = 0.1)[[1]], ncol=2)

grid.arrange(analz.samples(values, samples = samples, v = -0.2)[[1]],
             analz.samples(values, samples = samples, v = 0.2)[[1]], ncol=2)

grid.arrange(analz.samples(values, samples = samples, v = -0.3)[[1]],
             analz.samples(values, samples = samples, v = 0.3)[[1]], ncol=2)

grid.arrange(analz.samples(values, samples = samples, v = -0.4)[[1]],
             analz.samples(values, samples = samples, v = 0.4)[[1]], ncol=2)

grid.arrange(analz.samples(values, samples = samples, v = -0.5)[[1]],
             analz.samples(values, samples = samples, v = 0.5)[[1]], ncol=2)


# All

grid.arrange(analz.samples(values, samples = samples, v = -0.1)[[1]],
             analz.samples(values, samples = samples, v = 0.1)[[1]], 
             analz.samples(values, samples = samples, v = -0.2)[[1]],
             analz.samples(values, samples = samples, v = 0.2)[[1]], 
             analz.samples(values, samples = samples, v = -0.3)[[1]],
             analz.samples(values, samples = samples, v = 0.3)[[1]], 
             analz.samples(values, samples = samples, v = -0.4)[[1]],
             analz.samples(values, samples = samples, v = 0.4)[[1]], 
             ncol=4)


# observing only the positive values of v
grid.arrange(analz.samples(values, samples = samples, v = 0.1)[[1]], 
             analz.samples(values, samples = samples, v = 0.2)[[1]], 
             analz.samples(values, samples = samples, v = 0.3)[[1]], 
             analz.samples(values, samples = samples, v = 0.4)[[1]], 
             ncol=4)

ana.samples.paper.2 <- analz.samples(values, samples = samples, v = 0.4)[[1]] +
                       labs(title = "", y = "")


## plots to theta = 9

values <- list(N = 10000,
               n = 1000,
               mu = log(3),
               theta = 9)

filename <- paste('N', values$N,
                  'n', values$n,
                  'mu', round(values$mu,2),
                  'theta', round(values$theta,2),
                  sep='')
data_filename <-  paste(filename, '_data.RData',sep='')
load(paste(save_file, data_filename,sep=''))


samples <- data

grid.arrange(analz.samples(values, samples = samples, v = -0.1)[[1]],
             analz.samples(values, samples = samples, v = 0.1)[[1]], ncol=2)

grid.arrange(analz.samples(values, samples = samples, v = -0.2)[[1]],
             analz.samples(values, samples = samples, v = 0.2)[[1]], ncol=2)

grid.arrange(analz.samples(values, samples = samples, v = -0.3)[[1]],
             analz.samples(values, samples = samples, v = 0.3)[[1]], ncol=2)

grid.arrange(analz.samples(values, samples = samples, v = -0.4)[[1]],
             analz.samples(values, samples = samples, v = 0.4)[[1]], ncol=2)

grid.arrange(analz.samples(values, samples = samples, v = -0.5)[[1]],
             analz.samples(values, samples = samples, v = 0.5)[[1]], ncol=2)


# All

grid.arrange(analz.samples(values, samples = samples, v = -0.1)[[1]],
             analz.samples(values, samples = samples, v = 0.1)[[1]], 
             analz.samples(values, samples = samples, v = -0.2)[[1]],
             analz.samples(values, samples = samples, v = 0.2)[[1]], 
             analz.samples(values, samples = samples, v = -0.3)[[1]],
             analz.samples(values, samples = samples, v = 0.3)[[1]], 
             analz.samples(values, samples = samples, v = -0.4)[[1]],
             analz.samples(values, samples = samples, v = 0.4)[[1]], 
             ncol=4)


# observing only the positive values of v
grid.arrange(analz.samples(values, samples = samples, v = 0.1)[[1]], 
             analz.samples(values, samples = samples, v = 0.2)[[1]], 
             analz.samples(values, samples = samples, v = 0.3)[[1]], 
             analz.samples(values, samples = samples, v = 0.4)[[1]], 
             ncol=4)


ana.samples.paper.3 <- analz.samples(values, samples = samples, v = 0.4)[[1]] +
  labs(title = "", y = "")


grid.arrange(ana.samples.paper.1,ana.samples.paper.2,ana.samples.paper.3, nrow = 1)


## plots to theta = 100

values <- list(N = 10000,
               n = 1000,
               mu = log(3),
               theta = 100)

filename <- paste('N', values$N,
                  'n', values$n,
                  'mu', round(values$mu,2),
                  'theta', round(values$theta,2),
                  sep='')
data_filename <-  paste(filename, '_data.RData',sep='')
load(paste(save_file, data_filename,sep=''))

samples <- data

grid.arrange(analz.samples(values, samples = samples, v = -0.1)[[1]],
             analz.samples(values, samples = samples, v = 0.1)[[1]], ncol=2)

grid.arrange(analz.samples(values, samples = samples, v = -0.2)[[1]],
             analz.samples(values, samples = samples, v = 0.2)[[1]], ncol=2)

grid.arrange(analz.samples(values, samples = samples, v = -0.3)[[1]],
             analz.samples(values, samples = samples, v = 0.3)[[1]], ncol=2)

grid.arrange(analz.samples(values, samples = samples, v = -0.4)[[1]],
             analz.samples(values, samples = samples, v = 0.4)[[1]], ncol=2)

grid.arrange(analz.samples(values, samples = samples, v = -0.5)[[1]],
             analz.samples(values, samples = samples, v = 0.5)[[1]], ncol=2)


# All

grid.arrange(analz.samples(values, samples = samples, v = -0.1)[[1]],
             analz.samples(values, samples = samples, v = 0.1)[[1]], 
             analz.samples(values, samples = samples, v = -0.2)[[1]],
             analz.samples(values, samples = samples, v = 0.2)[[1]], 
             analz.samples(values, samples = samples, v = -0.3)[[1]],
             analz.samples(values, samples = samples, v = 0.3)[[1]], 
             analz.samples(values, samples = samples, v = -0.4)[[1]],
             analz.samples(values, samples = samples, v = 0.4)[[1]], 
             ncol=4)

ana.samples.paper.4 <- analz.samples(values, samples = samples, v = 0.4)[[1]] +
  labs(title = "", y = "")


grid.arrange(ana.samples.paper.1,ana.samples.paper.2,ana.samples.paper.3,ana.samples.paper.4, nrow = 1)



## Setting theta and change n

# n = 20

values <- list(N = 10000,
               n = 20,
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

p.n1 <- analz.samples(values, samples = samples, v = 0.4)[[2]] 


# n = 50

values <- list(N = 10000,
               n = 50,
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

p.n2 <- analz.samples(values, samples = samples, v = 0.4)[[2]] 


# n = 100

values <- list(N = 10000,
               n = 100,
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

p.n3 <- analz.samples(values, samples = samples, v = 0.4)[[2]] 


# n = 500

values <- list(N = 10000,
               n = 500,
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

p.n4 <- analz.samples(values, samples = samples, v = 0.4)[[2]] 


grid.arrange(p.n1,p.n3,p.n4,ncol = 3)




