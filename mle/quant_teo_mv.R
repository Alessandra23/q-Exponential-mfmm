quan.teo.mv <- function(values){
  
  n <- values$n
  N <- values$N
  mu <- values$mu
  theta <- values$theta
  
  q.real <- (theta+3)/(theta+2)
  samples <- qexp.samples(N=N,n=n,theta=theta,mu=mu)
  est.mv <- t(sapply(1:nrow(samples),est.mv,samples=samples)) 
  est.mv.mu <- est.mv[,1]
  est.mv.theta <- est.mv[,2]
  est.mv.q <- (3+est.mv.theta)/(2+est.mv.theta)
  var.mu <- solve(K.mu.theta(c(mu,theta)))[1,1] ## Of mu
  var.theta <- solve(K.mu.theta(c(mu,theta)))[2,2] ## Of theta
  mu.pad.mv <- sqrt(n)*(est.mv.mu-mu)/sqrt(var.mu)
  theta.pad.mv <- sqrt(n)*(est.mv.theta-theta)/sqrt(var.theta)
  q.pad.mv <- sqrt(n)*(est.mv.q-q.real)/sqrt(var.theta*((theta+2)^(-4)))
  
  #df.est.mv <- data.frame(est.mv.mu = est.mv.mu, est.mv.theta = est.mv.theta, est.mv.q = est.mv.q, 
  #                        mu.pad.mv = mu.pad.mv, theta.pad.mv = theta.pad.mv, q.pad.mv = q.pad.mv)
  df.est.mv <- data.frame(est.mv.mu , est.mv.theta , est.mv.q,  
                          mu.pad.mv , theta.pad.mv , q.pad.mv)
  
  
  p.mu.mv <- ggplot(df.est.mv) +
    geom_density(aes(x=est.mv.mu),size=1) + xlim(0,25)+
    labs( x = expression(hat(mu)),y = "Density")
  
  p.theta.mv <- ggplot(df.est.mv) +
    geom_density(aes(x=est.mv.theta),size=1) +xlim(0,25)+
    labs( x = expression(hat(theta)),y = "Density")
  
  
  p.q.mv <- ggplot(df.est.mv) +
    geom_density(aes(x=est.mv.q),size=1) +
    labs( x = expression(hat(q)),y = "Density")
  
  
  return(list(df.est.mv = df.est.mv,
              p.mu.mv = p.mu.mv, p.theta.mv = p.theta.mv, p.q.mv = p.q.mv))
  
}


# mu = log(3), theta = 1/9, n = 50

values <- list(N = 10000,
               n = 50,
               mu = log(3),
               theta = 1)


plots.mv.ind <- quan.teo.mv(values = values)
plots.mv.ind$p.mu.mv


# mu = 10, theta = 1/9, n = 50

values <- list(N = 10000,
               n = 50,
               mu = 10,
               theta = 1)


plots.mv.ind <- quan.teo.mv(values = values)
plots.mv.ind$p.mu.mv




