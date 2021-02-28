#' mfmm.theo
#'
#' @description Theoretical quantities for the mfmm
#' @import GoFKernel
#' @import numDeriv
#' @import tidyverse
#'
#' @export
mfmm.theo <- function(n, mu, theta, v, u = -v, samples, d=1e-10){

  g.theta.n <- function(theta0){(theta0+1)^(u-v)*beta(v+1,theta0-v+1)^u*beta(u+1,theta0-u+1)^(-v)}
  # Inverse of g_theta
  g.theta.inv <- inverse(g.theta.n,lower=0,upper=10000)

  # theoretical quantities
  ev <- (mu*theta)^v*(theta+1)*beta(v+1,theta+1-v)
  eu <- (mu*theta)^u*(theta+1)*beta(u+1,theta+1-u)
  sig.v <- (mu*theta)^(2*v)*(theta+1)*(beta(2*v+1,theta+1-2*v)-(theta+1)*(beta(v+1,theta+1-v)^2))
  sig.u <- (mu*theta)^(2*u)*(theta+1)*(beta(2*u+1,theta+1-2*u)-(theta+1)*(beta(u+1,theta+1-u)^2))
  sig.uv <- ((mu*theta)^(u+v))*(theta+1)*(beta(u+v+1,theta+1-u-v)-(theta+1)*beta(u+1,theta+1-u)*beta(v+1,theta+1-v))
  p1 <- (u^2)*sig.v*ev^(2*u-2)*eu^(-2*v)
  p2 <- u*v*ev^(2*u-1)*eu^(-2*v-1)*sig.uv
  p3 <- (v^2)*sig.u*ev^(2*u)*eu^(-2*v-2)
  gamma2 <- p1-2*p2+p3
  Esp.Tn <- (ev^u)/(eu^v)

  inv.esp <- g.theta.inv(Esp.Tn)
  der.inv <- grad(g.theta.inv,Esp.Tn, method.args = list(eps=1e-12,d=d,r=6))

  lim.inf <- ((gamma(v)*v)^u/((gamma(u)*u)^v))*((-gamma(-v)*v)^u/((-gamma(-u)*u)^v))
  lim.sup <- (10000+1)^(u-v)*beta(v+1,10000-v+1)^u*beta(u+1,10000-u+1)^(-v)

  # T_v^u/T_u^v
  mu.uv <- (rowMeans(samples^v)^u)/(rowMeans(samples^u)^v)
  # Selecionando a amostra
  ifelse(v>0,samp.cond <- mu.uv[mu.uv > lim.inf & mu.uv < lim.sup],
         samp.cond <- mu.uv[mu.uv < lim.inf & mu.uv > lim.sup])
  # theta estimado com base na amostra selecionada
  theta.hat <- sapply(samp.cond, g.theta.inv)
  # variância obtida pelo método delta para a inversa da função
  kappa2 <- gamma2*(der.inv)^2
  # padronizando o theta estimado
  theta.hat.pad <- (sqrt(n)*(theta.hat-theta))/sqrt(kappa2)
  # Estimativas de q
  q.hat <- (3+theta.hat)/(2+theta.hat)
  # Variância assintótica de q
  var.qhat <- (inv.esp+2)^(-4)*kappa2
  # Valor verdadeiro de q
  q.real <- (3+theta)/(2+theta)
  # padronizando o q estimado
  q.hat.pad <- (sqrt(n)*(q.hat-q.real))/sqrt(var.qhat)

  #d.q.theta <- data.frame(theta.hat,theta.hat.pad,q.hat,q.hat.pad)

  return(list(theta.hat = theta.hat,theta.hat.pad = theta.hat.pad,q.hat=q.hat, q.hat.pad= q.hat.pad,kappa2 = kappa2,lim.sup = lim.sup))

}



#' Select only theta.pad
gen_q_pad <- function(samples, values, theta){

  n <- values$n
  mu <- values$mu
  theta <- values$theta

  q.hat.pad <- mfmm.theo(n = n, mu = mu, theta = theta, v = v, u = -v, samples = samples)$q.hat.pad

  return(q.hat.pad)
}


