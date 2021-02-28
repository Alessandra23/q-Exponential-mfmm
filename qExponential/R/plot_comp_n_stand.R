#' Function to create the plot of theta stan comp n
#'
#' @import ggplot2
#' @export
comp.n.q.pad <- function(values, n.values, v.values){

  y <- lapply(v.values, function(values, v, n){

    N <- values$N
    mu <- values$mu
    theta <- values$theta
    n <- n

    x <- lapply(n, gen_q_pad, N = N, v = v, mu = mu, theta = theta)
    names(x) <- n

    return(x)

  }, values = values, n = n.values)


  names(y) <- v.values
  y <- melt(y)

  p <- ggplot(melt(y),aes(x = value, group = L2)) +
    geom_density(aes(linetype=factor(L2)))+
    facet_wrap( ~ L1, labeller = label_bquote(paste("v = ",.(L1))))+
    stat_function(fun = dnorm,n=101, args = list(mean = 0, sd = 1),aes(linetype="Normal")) +
    scale_linetype_manual(name="n",values=c("dashed","dotdash", "dotted","solid")) +
    labs(y = "Density", x = expression(hat(q)[MFP]))+
    xlim(c(-5,5)) + theme_bw()

  return(p)
}
