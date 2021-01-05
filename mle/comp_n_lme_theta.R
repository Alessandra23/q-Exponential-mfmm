# Compare sample sizes to standardized theta MLE.
theme_set(theme_bw())

quan.teo.mv.theta <- function(n, N, mu, theta){
  
  samples <- qexp.samples(N=N,n=n,theta=theta,mu=mu)
  est.mv <- t(sapply(1:nrow(samples),est.mv,samples=samples)) 
  est.mv.theta <- est.mv[,2]
  var.theta <- solve(K.mu.theta(c(mu,theta)))[2,2] 
  theta.pad.mv <- sqrt(n)*(est.mv.theta-theta)/sqrt(var.theta)
  
  return(theta.pad.mv)
}


comp.n.theta.pad <- function(values, n.values, theta.values){
  
  y <- lapply(theta.values, function(values, theta, n){
    
    N <- values$N
    mu <- values$mu
    n <- n
    
    x <- lapply(n, quan.teo.mv.theta, N = N, mu = mu, theta = theta)
    names(x) <- n
    
    return(x)
    
  }, values = values, n = n.values)
  
  
  names(y) <- theta.values
  y <- melt(y)
  
  p <- ggplot(melt(y),aes(x = value, group = L2)) +
    geom_density(aes(linetype=factor(L2)))+ 
    facet_wrap( ~ L1, labeller = label_bquote(paste(theta," = ",.(L1))))+ 
    stat_function(fun = dnorm,n=101, args = list(mean = 0, sd = 1),aes(linetype="Normal")) +
    scale_linetype_manual(name="n",values=c("dashed","dotdash", "dotted","solid")) +
    labs(y = "Density", x = expression(hat(theta)[MLP]))+
    xlim(c(-5,5))
  
  return(p)
}



values <- list(N = 10,
               mu = log(3))
n.values = c(50, 500, 1000)
theta.values <- c(round(1/9,2),1,9)

comp.n.theta.pad(values = values, n.values = n.values, theta.values = theta.values)


