#' g(theta) function
#'
#' @param u
#' @param v
#' @param theta
#' @export


g.theta <- function(theta,v,u = -v){
  (theta+1)^(u-v)*beta(v+1,theta-v+1)^u*beta(u+1,theta-u+1)^(-v)
}
