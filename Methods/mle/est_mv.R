## Function to obtain a pair of estimates

est.mv <- function(samples,i){
  
  x <- samples[i,]
  mu.est <- mean(x)
  theta.est <- mean(x^0.1)/(mean(x)^0.1)
  est.mu.theta <- optim(par = c(mu.est,theta.est), log.qexp, x = x)
  
  return(est.mu.theta$par)
}


# est.mv(samples, 1)