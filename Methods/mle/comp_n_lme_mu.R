# Compare sample sizes to standardized mu.
theme_set(theme_bw())

quan.teo.mv.mu <- function(n, N, mu, theta){
  
  samples <- qexp.samples(N=N,n=n,theta=theta,mu=mu)
  est.mv <- t(sapply(1:nrow(samples),est.mv,samples=samples)) 
  est.mv.mu <- est.mv[,1]
  var.mu <- solve(K.mu.theta(c(mu,theta)))[1,1] ## Of mu
  mu.pad.mv <- sqrt(n)*(est.mv.mu-mu)/sqrt(var.mu)
  
  return(mu.pad.mv)
}


comp.n.mu.pad <- function(values, n.values, mu.values){
  
  y <- lapply(mu.values, function(values, mu, n){
    
    N <- values$N
    theta <- values$theta
    n <- n
    
    x <- lapply(n, quan.teo.mv.mu, N = N, mu = mu, theta = theta)
    names(x) <- n
    
    return(x)
    
  }, values = values, n = n.values)
  
  
  names(y) <- mu.values
  y <- melt(y)
  
  p <- ggplot(melt(y),aes(x = value, group = L2)) +
    geom_density(aes(linetype=factor(L2)))+ 
    facet_wrap( ~ L1, labeller = label_bquote(paste(mu," = ",.(L1))))+ 
    stat_function(fun = dnorm,n=101, args = list(mean = 0, sd = 1),aes(linetype="Normal")) +
    scale_linetype_manual(name="n",values=c("dashed","dotdash", "dotted","solid")) +
    labs(y = "Density", x = expression(hat(mu)[MLP]))+
    xlim(c(-5,5))
  
  return(p)
}



values <- list(N = 10000,
               theta = 1/9)
n.values = c(50, 500, 1000)
mu.values <- c(round(1/9,1),round(log(3),1),10)

comp.n.mu.pad(values = values, n.values = n.values, mu.values = mu.values)


