## Theoretical variances

var.teo.mv <- function(values){
  
  mu <- values$mu
  theta <- values$theta
  
  var.mu <- solve(K_mu_theta(c(mu,theta)))[1,1] 
  var.theta <- solve(K_mu_theta(c(mu,theta)))[2,2]
  
  return(list(var.mu,var.theta))
}