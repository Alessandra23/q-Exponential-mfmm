#' Analyze the value of n using the theoretical quantities
#'
#' @import tidyverse
#' @import ggplot2
#' @param n
#' @param values
#' @param v
#' @param u
#' @export
#'
prob.asym <- function(n,mu, theta,v, u=-v){

  # theoretical quantities
  lim.sup <- (10000+1)^(u-v)*beta(v+1,10000-v+1)^u*beta(u+1,10000-u+1)^(-v)
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

  x.lim1 <- (1.88*sqrt(gamma2)/(lim.sup-Esp.Tn))^2
  x.lim2 <- (10*sqrt(gamma2)/(lim.sup-Esp.Tn))^2

  prob.M <- pnorm(sqrt(n)*(lim.sup-Esp.Tn)/sqrt(gamma2))


  return(list(prob.M, x.lim1,x.lim2))
}


#'
#'
#' Graph for n and the probability of rejecting

#' @export
plot.reject <- function(n, mu, theta, v){

  lim <- unlist(prob.asym(n = n, mu = mu , theta = theta, v=v)[2])
  lim <- ifelse(lim < 200,200,lim)
  lim <- seq(0,lim,length=1000)


  mat.pr <- sapply(c(0.1,0.2,0.3,0.4),function(i){
    sapply(lim, prob.asym, mu = mu, theta = theta ,v=i)[1,]
  })

  df.pr <- gather(mat.pr %>% as.data.frame())


  df.pr <- df.pr %>%
    mutate(key=case_when(
      key=="V1"~0.1,
      key=="V2"~0.2,
      key=="V3"~0.3,
      TRUE ~ 0.4)
    ) %>% cbind(x=rep(lim,4))

  df.pr$x <- unlist(df.pr$x)
  df.pr$value <- unlist(df.pr$value)

  p.pr <-   ggplot(df.pr, aes(x=x,y=value)) +
    geom_line() +
    facet_wrap(~key, labeller = label_bquote(paste("v = ",.(key))), scales = "fixed") +
    labs(x=expression(n), y = expression(Phi(sqrt(n)*(l[uv]-mu[uv])/kappa))) +
    geom_hline(yintercept = 0.97, linetype = 5,size=0.5) + theme_bw()

  return(p.pr)

}
