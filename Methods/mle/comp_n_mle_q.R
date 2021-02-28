# Compare sample sizes to standardized q MLE.
theme_set(theme_bw())

source("qexp_functions.R")
source("qexp_generate_samples.R")

quan.teo.mv.q <- function(n, N, mu, theta){
  
  q.real <- (theta+3)/(theta+2)
  samples <- qexp.samples(N=N,n=n,theta=theta,mu=mu)
  est.mv <- t(sapply(1:nrow(samples),est.mv,samples=samples)) 
  est.mv.theta <- est.mv[,2]
  est.mv.q <- (3+est.mv.theta)/(2+est.mv.theta)
  var.theta <- solve(K.mu.theta(c(mu,theta)))[2,2] 
  q.pad.mv <- sqrt(n)*(est.mv.q-q.real)/sqrt(var.theta*((theta+2)^(-4)))
  
  return(q.pad.mv)
}


comp.n.q.pad <- function(values, n.values, q.values){
  
  y <- lapply(q.values, function(values, theta, n){
    
    N <- values$N
    mu <- values$mu
    n <- n
    
    x <- lapply(n, quan.teo.mv.q, N = N, mu = mu, theta = theta)
    names(x) <- n
    
    return(x)
    
  }, values = values, n = n.values)
  
  
  names(y) <- q.values
  y <- melt(y)
  
  p <- ggplot(melt(y),aes(x = value, group = L2)) +
    geom_density(aes(linetype=factor(L2)))+ 
    facet_wrap( ~ L1, labeller = label_bquote(paste(" q = ",.(L1))))+ 
    stat_function(fun = dnorm,n=101, args = list(mean = 0, sd = 1),aes(linetype="Normal")) +
    scale_linetype_manual(name="n",values=c("dashed","dotdash", "dotted","solid")) +
    labs(y = "Density", x = expression(hat(q)[MLP]))
  
  return(p)
}



values <- list(N = 100,
               mu = log(3))
n.values = c(20, 50, 100)
theta.values <- c(round(1/9,1),1,9)
q.values <- round((theta.values+3)/(theta.values+2),2)

comp.n.q.pad(values = values, n.values = n.values, q.values = q.values)


