#' Plots of the function g(theta) with samples and asymptotes.
#' 
#' 

library(gridExtra)
library(ggplot2)
theme_set(theme_bw())

source("qexp_functions.R")
source("qexp_generate_samples.R")
source("mfmm/gtheta_function.R")
source("mfmm/analz.samples.R")

## plots to theta = 1/9

values <- list(N = 10000,
               n = 1000,
               mu = log(3),
               theta = 1/9)

samples <- qexp.samples(N=values$N,n=values$n,theta=values$theta,mu=values$mu)

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


## plots to theta = 1

values <- list(N = 10000,
               n = 1000,
               mu = log(3),
               theta = 1)

samples <- qexp.samples(N=values$N,n=values$n,theta=values$theta,mu=values$mu)

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



## plots to theta = 6

values <- list(N = 10000,
               n = 1000,
               mu = log(3),
               theta = 6)

samples <- qexp.samples(N=values$N,n=values$n,theta=values$theta,mu=values$mu)

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



## plots to theta = 100

values <- list(N = 10000,
               n = 1000,
               mu = log(3),
               theta = 100)

samples <- qexp.samples(N=values$N,n=values$n,theta=values$theta,mu=values$mu)

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



## Setting theta and change n

# n = 20

values <- list(N = 10000,
               n = 20,
               mu = log(3),
               theta = 1/9)

samples <- qexp.samples(N=values$N,n=values$n,theta=values$theta,mu=values$mu)
p.n1 <- analz.samples(values, samples = samples, v = 0.4)[[2]] 


# n = 50

values <- list(N = 10000,
               n = 50,
               mu = log(3),
               theta = 1/9)

samples <- qexp.samples(N=values$N,n=values$n,theta=values$theta,mu=values$mu)
p.n2 <- analz.samples(values, samples = samples, v = 0.4)[[2]] 


# n = 100

values <- list(N = 10000,
               n = 100,
               mu = log(3),
               theta = 1/9)

samples <- qexp.samples(N=values$N,n=values$n,theta=values$theta,mu=values$mu)
p.n3 <- analz.samples(values, samples = samples, v = 0.4)[[2]] 


# n = 500

values <- list(N = 10000,
               n = 500,
               mu = log(3),
               theta = 1/9)

samples <- qexp.samples(N=values$N,n=values$n,theta=values$theta,mu=values$mu)
p.n4 <- analz.samples(values, samples = samples, v = 0.4)[[2]] 


grid.arrange(p.n1,p.n3,p.n4,ncol = 3)




