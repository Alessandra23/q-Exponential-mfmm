# Expectited Fisher Information

K.mu.theta <- function(values){
  
  mu <- values[1]
  theta <- values[2]
  
  KF <- matrix(0,2,2)
  KF[1,1] <- ((theta+2)^2)/(mu^2*(theta^2+5*theta+6))
  KF[2,2] <- (theta^2+theta+2)/(theta^2*(theta+1)^2*(theta^2+5*theta+6))
  KF[1,2] <- 2/(mu*theta*(theta^2+5*theta+6))
  KF[2,1] <- KF[1,2]
  
  return(KF)
}
