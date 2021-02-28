#' analz.samples
#'
#' @description Function to analyze samples
#'
#' @importFrom  GoFKernel 'inverse'
#' @import numDeriv
#' @param values
#' @param v
#' @param u
#' @param samples
#'
#' @return
#'
#' @example analz.samples(values = list(1,1,2,3),samples = qexp.samples(2,3,1,1), v = 0.1)
#'
#' @export
#'
analz.samples <- function(values, samples, v, u = -v){

  mu <- values$mu
  theta <- values$theta
  n <- values$n
  N <- values$N

  #
  g.theta.n <- function(theta0){(theta0+1)^(u-v)*beta(v+1,theta0-v+1)^u*beta(u+1,theta0-u+1)^(-v)}
  # Inverse of g_theta
  g.theta.inv <- inverse(g.theta.n,lower=0,upper=10000)

  # T_v^u/T_u^v
  mu.uv <- (rowMeans(samples^v)^u)/(rowMeans(samples^u)^v)

  # Limit of g(theta) when theta tends to zero
  lim.inf <- ((gamma(v)*v)^u/((gamma(u)*u)^v))*((-gamma(-v)*v)^u/((-gamma(-u)*u)^v))
  #Upper limit (asymptote)
  lim.sup <- (10000+1)^(u-v)*beta(v+1,10000-v+1)^u*beta(u+1,10000-u+1)^(-v)

  # Proportion of rejection
  prop.rejec <- sum(ifelse(mu.uv > lim.sup,1,0))/N


  # Plot g(theta) and the values of the statistic Tn = T_v^u/T_u^v
  p <- ggplot(data = data.frame(x = 0,z=lim.sup, w = mu.uv), mapping = aes(x = x,w)) +
    stat_function(fun = g.theta.n) + xlim(0,500) +
    labs(title = bquote(paste("v = ", .(v),", u = ", .(u))),x=expression(theta), y = bquote(paste("g(",theta,")"))) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.55, size = rel(1)))+
    geom_hline(yintercept = lim.sup, linetype = 5,size=0.5)+
    geom_point(colour = "black", size = 0.25)

  p.n <- ggplot(data = data.frame(x = 0,z=lim.sup, w = mu.uv), mapping = aes(x = x,w)) +
    stat_function(fun = g.theta.n) + xlim(0,500) +
    labs(title = bquote(paste("n = ", .(n),", Rejected proportion = ",.(prop.rejec))),x=expression(theta), y = bquote(paste("g(",theta,")"))) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.55, size = rel(1)))+
    geom_hline(yintercept = lim.sup, linetype = 5,size=0.5)+
    geom_point(colour = "black", size = 0.3)


  return(list(p, p.n, mu.uv, lim.inf, lim.sup, prop.rejec))
}

