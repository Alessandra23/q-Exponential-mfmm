##############################  Estimação via Método dos Momentos Fracionários Modificado ##############################  

############### Packages ###############
library(GoFKernel) ## Calcular a inversa
library(numDeriv)  ## Calcular as derivadas
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(magrittr)
library(ggformula)
library(lemon)
library(lattice)
#rm(list = ls())
#.rs.restartR()
theme_set(theme_bw())


##############################  Misc. functions ##############################

############### Generate qExp functions  ###############
tsal.shape.from.q <- function(q) {
  shape <- -1/(1-q)
  return(shape)
}
tsal.scale.from.qk <- function(q,kappa) {
  shape <- tsal.shape.from.q(q)
  scale <- shape*kappa
  return(scale)
}
tsal.q.from.shape <- function(shape) {
  q <- 1+1/shape
  return(q)
}
tsal.kappa.from.ss <- function(shape,scale) {
  kappa <- scale/shape
  return(kappa)
}
tsal.ss.from.qk <- function(q,kappa) {
  ss <- c(tsal.shape.from.q(q),tsal.scale.from.qk(q,kappa))
  return(ss)
}
tsal.qk.from.ss <- function(shape,scale) {
  qk <- c(tsal.q.from.shape(shape),tsal.kappa.from.ss(shape,scale))
  return(qk)
}
dtsal <- function(x, shape=1,scale=1, q=tsal.q.from.shape(shape),kappa=tsal.kappa.from.ss(shape,scale), xmin=0,log=FALSE){
  # If we have both shape/scale and q/kappa parameters,
  # the latter over-ride.
  ss <- tsal.ss.from.qk(q,kappa)
  shape <- ss[1]
  scale <- ss[2]
  # Under censoring, pass off to the tail version
  if (xmin > 0) { return(dtsal.tail(x, shape, scale, q, kappa, xmin, log)) }
  z <- 1+x/scale
  if (log) { d <- -(shape+1)*log(z) + log(shape/scale) }
  else { d <- (shape/scale)*(z^(-shape-1)) }
  return(d)
}
ptsal <- function(x, shape=1, scale=1, q=tsal.q.from.shape(shape),kappa=tsal.kappa.from.ss(shape,scale), xmin=0,lower.tail=TRUE, log.p=FALSE) {
  # If we have both shape/scale and q/kappa parameters,
  # the latter over-ride.
  ss <- tsal.ss.from.qk(q,kappa)
  shape <- ss[1]
  scale <- ss[2]
  if (xmin > 0) { return(ptsal.tail(x, shape, scale, q, kappa, xmin, lower.tail,
                                    log.p)) }
  z <- 1+x/scale
  if ((log.p) && (!lower.tail)) { p <- -shape*log(z) }
  if ((log.p) && (lower.tail)) { p <- log(1-(z^(-shape))) }
  if ((!log.p) && (!lower.tail)) { p <- z^(-shape) }
  if ((!log.p) && (lower.tail)) { p <- 1 - z^(-shape) }
  return(p)
}
qtsal <- function(p,  shape=1, scale=1, q=tsal.q.from.shape(shape),kappa=tsal.kappa.from.ss(shape,scale), xmin=0,lower.tail=TRUE, log.p=FALSE) {
  # If we have both shape/scale and q/kappa parameters,
  # the latter over-ride.
  ss <- tsal.ss.from.qk(q,kappa)
  shape <- ss[1]
  scale <- ss[2]
  if (xmin > 0) { return(qtsal.tail(p, shape, scale, q, kappa, xmin, lower.tail,
                                    log.p)) }
  if (log.p) { p <- exp(p) }
  if (lower.tail) { p <- 1-p }
  # The upper quantile function is given by
  # (scale)(p^{-1/shape} - 1) = x
  quantiles <- scale*(-1 + (p^(-1/shape)))
  return(quantiles)
}
rtsal <- function(n, shape=1, scale=1, q=tsal.q.from.shape(shape),kappa=tsal.kappa.from.ss(shape,scale), xmin=0){
  # If we have both shape/scale and q/kappa parameters, the latter over-ride.
  ss <- tsal.ss.from.qk(q,kappa)
  shape <- ss[1]
  scale <- ss[2]
  if (xmin > 0) { return(rtsal.tail(n, shape, scale, q, kappa, xmin)) }
  # Apply the transformation method
  ru <- runif(n)
  r <- qtsal(ru,shape,scale)
  return(r)
}



############### Generate samples ###############
G.samples<-function(N,nn,theta,mu){
  MC_sim<-lapply(nn,rep,each=N)
  result<-lapply(1:length(nn),function(i){
    t(sapply(MC_sim[[i]] , rtsal, shape = theta+1, scale = mu*theta))})
  return(result)
}

############### Valores iniciais ###############

n0 <- 1000 ## Tamanho amostral
theta0 <- 1 ## Valor de theta
mu0 <- log(3) ## Valor de mu
v0 <- 0.1 ## Valor de v
u0 <- -v0 ## Valor de u (por padrão -v)

############### Limites para g(theta) ###############
liminf <- function(v,u) ((gamma(v)*v)^u/((gamma(u)*u)^v))*((-gamma(-v)*v)^u/((-gamma(-u)*u)^v)) ## Função para obter o limite inferior
limsup <- function(v,u){(100+1)^(u-v)*beta(v+1,100-v+1)^u*beta(u+1,100-u+1)^(-v)} ## Função para o limite inferior (assíntota)



############### Estudar a função g(theta) ###############

############### Função para plotar individualmente os gráficos dado u e v.############### 
g_theta_func  <- function(v,u=-v){
  g_theta <- function(theta0){(theta0+1)^(u-v)*beta(v+1,theta0-v+1)^u*beta(u+1,theta0-u+1)^(-v)} ## Função de theta que preciso obter a raiz
  lim_inf <- ((gamma(v)*v)^u/((gamma(u)*u)^v))*((-gamma(-v)*v)^u/((-gamma(-u)*u)^v))## Limite de g_theta quando theta tende a zero
  limsup <- (1000000000+1)^(u-v)*beta(v+1,1000000000-v+1)^u*beta(u+1,1000000000-u+1)^(-v) ## Limite superior (assíntota)
  p1 <- ggplot(data = data.frame(x = 0,z=limsup), mapping = aes(x = x)) + stat_function(fun = g_theta) + xlim(0,1000) + 
        #annotate("text", x=500, y=g_theta(0.64), label= paste("u = ", u, ", v = ", v),size = 4, colour = "red",fontface = "bold") +
        labs(title = bquote(paste("u = ", .(u), ", v = ", .(v))),x=expression(theta), y = bquote(paste("g(",theta,")"))) +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5))+
        geom_hline(yintercept = limsup, color = "red", linetype = 5,size=0.8)
  return(list(p1,limsup))  
}


grid.arrange(g_theta_func(v=0.1)[[1]], g_theta_func(v=-0.1)[[1]], ncol=2) ## COnsiderando um valor particular de u e v
grid.arrange(g_theta_func(v=0.2)[[1]], g_theta_func(v=-0.2)[[1]], ncol=2) ## COnsiderando um valor particular de u e v
grid.arrange(g_theta_func(v=0.3)[[1]], g_theta_func(v=-0.3)[[1]], ncol=2) ## COnsiderando um valor particular de u e v
grid.arrange(g_theta_func(v=0.4)[[1]], g_theta_func(v=-0.4)[[1]], ncol=2) ## COnsiderando um valor particular de u e v
grid.arrange(g_theta_func(v=0.5)[[1]], g_theta_func(v=-0.5)[[1]], ncol=2) ## COnsiderando um valor particular de u e v


grid.arrange(g_theta_func(v=0.7)[[1]], g_theta_func(v=-0.7)[[1]],
             g_theta_func(v=0.2)[[1]], g_theta_func(v=-0.2)[[1]],
             g_theta_func(v=0.3)[[1]], g_theta_func(v=-0.3)[[1]],
             g_theta_func(v=0.4)[[1]], g_theta_func(v=-0.4)[[1]],
             g_theta_func(v=0.5)[[1]], g_theta_func(v=-0.5)[[1]], ncol=2) ## COnsiderando um valor particular de u e v



##### Reunir em um único gráfico todas as curvas de u e v

g_theta <- function(theta0){(theta0+1)^(u-v)*beta(v+1,theta0-v+1)^u*beta(u+1,theta0-u+1)^(-v)} ## Função de theta que preciso obter a raiz

myV<-seq(0.1,0.5,0.1)

list_y<-lapply(myV,function(V){
  assign("v", V, envir = .GlobalEnv)
  assign("u", -V, envir = .GlobalEnv)
  sapply(0:100,function(x){
    result<-g_theta(x)
    return(result)
  })  
})

mydf<-data.frame(y=unlist(list_y),x=rep(0:100,length(myV)),z=factor(rep(myV,each = 101))) %>% 
  set_names(c("y","x","v"))
p2 <- mydf %>% 
  ggplot(aes(x=x,y=y))+
  geom_line(aes(linetype=v)) +
  labs(x=expression(theta), y = bquote(paste("g(",theta,")"))) +theme_bw() 

myV2<- seq(-0.1,-0.5,-0.1)
list_y2<-lapply(myV2,function(V){
  assign("v", V, envir = .GlobalEnv)
  assign("u", -V, envir = .GlobalEnv)
  sapply(0:100,function(x){
    result<-g_theta(x)
    return(result)
  })  
})
mydf2<-data.frame(y=unlist(list_y2),x=rep(0:100,length(myV)),z=factor(rep(myV2,each = 101))) %>% 
  set_names(c("y","x","v"))
p3 <- mydf2 %>% 
  ggplot(aes(x=x,y=y))+
  geom_line(aes(linetype=v)) +
  labs(x=expression(theta), y = bquote(paste("g(",theta,")"))) +theme_bw() 
grid.arrange(p2,p3)


############### Estudar a inversa de g(theta) ###############


## Revisar essa função

g_theta_inv_func  <- function(v,u=-v, Leng){
  g_theta <- function(theta0){(theta0+1)^(u-v)*beta(v+1,theta0-v+1)^u*beta(u+1,theta0-u+1)^(-v)}
  g_theta_inv<-inverse(g_theta,lower=0,upper=0.5)
  lim_inf <- liminf(v=v,u=u)
  lim_sup <- limsup(v=v,u=u)
  ifelse(v>0, valores <- seq(g_theta(0.05),lim_sup,Leng),valores <- seq(g_theta(0.05),lim_inf,Leng))
  inv_app <- sapply(valores,g_theta_inv)
  df_inv <- data.frame(valores,inv_app)
  p2 <- df_inv %>% 
        ggplot(aes(x=valores,y=inv_app))+
        geom_line() +
        labs(title = bquote(paste("u = ", .(u), ", v = ", .(v))),x="x", y = expression(paste(g^{-1},"(x)"))) +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5))
  ## O que a função g_theta_func deve retornar
  return(list(p2,inv_app,valores))
}







############### Analisar as amostras ###############

analz_samples <- function(mu,theta,v,u=-v,n=ncol(samples),N=10000,mysample){
  g_theta <- function(theta0){(theta0+1)^(u-v)*beta(v+1,theta0-v+1)^u*beta(u+1,theta0-u+1)^(-v)} ## We want to have the inverse of this function 
  g_theta_inv<-inverse(g_theta,lower=0,upper=10000) # Inverse of g_theta
  mu_uv <- (rowMeans(mysample^v)^u)/(rowMeans(mysample^u)^v) # T_v^u/T_u^v
  lim_inf <- ((gamma(v)*v)^u/((gamma(u)*u)^v))*((-gamma(-v)*v)^u/((-gamma(-u)*u)^v))## Limite de g_theta quando theta tende a zero
  limsup <- (1000000000+1)^(u-v)*beta(v+1,1000000000-v+1)^u*beta(u+1,1000000000-u+1)^(-v) ## Limite superior (assíntota)
  ### Gráfico com g(theta) e os valores da estatística Tn = T_v^u/T_u^v
  p3 <- ggplot(data = data.frame(x = 0,z=limsup, w = mu_uv), mapping = aes(x = x,w)) +
    stat_function(fun = g_theta) + xlim(0,500) + 
    #annotate("text", x=500, y=g_theta(0.64), label= paste("u = ", u, ", v = ", v),size = 4, colour = "red",fontface = "bold") +
    labs(title = bquote(paste("u = ", .(u), ", v = ", .(v))),x=expression(theta), y = bquote(paste("g(",theta,")"))) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5))+
    geom_hline(yintercept = limsup, linetype = 5,size=0.5)+
    geom_point(colour = "black", size = 0.3)
  prop_rejec <-   sum(ifelse(mu_uv>limsup,1,0))/N
  return(list(p3,prop_rejec))
}



theta0=1/9
n0= 1000
samples <- do.call(as.matrix, G.samples(N=10000,n=n0,theta=theta0,mu=mu0))# Amostras geradas com os valores de n, N, theta e mu.
analz_samples(mu=mu0,theta=theta0,v=0.1,mysample = samples)





## Gráficos para theta0 = 1/9

theta0 <- 1/9
samples <- do.call(as.matrix, G.samples(N=10000,n=n0,theta=theta0,mu=mu0))# Amostras geradas com os valores de n, N, theta e mu.
grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.1,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.1,mysample = samples)[[1]], ncol=2) 
grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.2,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.2,mysample = samples)[[1]], ncol=2) 
grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.3,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.3,mysample = samples)[[1]], ncol=2) 
grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.4,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.4,mysample = samples)[[1]], ncol=2) 
#grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.5,mysample = samples)[[1]],
#             analz_samples(mu=mu0,theta=theta0,v=0.5,mysample = samples)[[1]], ncol=2) 


grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.1,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.1,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=-0.2,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.2,mysample = samples)[[1]], 
             analz_samples(mu=mu0,theta=theta0,v=-0.3,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.3,mysample = samples)[[1]], 
             analz_samples(mu=mu0,theta=theta0,v=-0.4,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.4,mysample = samples)[[1]], ncol=4) 
#grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.5,mysample = samples)[[1]],
#             analz_samples(mu=mu0,theta=theta0,v=0.5,mysample = samples)[[1]], ncol=2) 



## Gráficos para theta0 = 1

theta0 <- 1
samples <- do.call(as.matrix, G.samples(N=10000,n=n0,theta=theta0,mu=mu0))# Amostras geradas com os valores de n, N, theta e mu.
grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.1,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.1,mysample = samples)[[1]], ncol=2) 
grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.2,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.2,mysample = samples)[[1]], ncol=2) 
grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.3,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.3,mysample = samples)[[1]], ncol=2) 
grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.4,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.4,mysample = samples)[[1]], ncol=2) 
#grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.5,mysample = samples)[[1]],
#             analz_samples(mu=mu0,theta=theta0,v=0.5,mysample = samples)[[1]], ncol=2) 


grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.1,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.1,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=-0.2,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.2,mysample = samples)[[1]], 
             analz_samples(mu=mu0,theta=theta0,v=-0.3,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.3,mysample = samples)[[1]], 
             analz_samples(mu=mu0,theta=theta0,v=-0.4,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.4,mysample = samples)[[1]], ncol=4) 
#grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.5,mysample = samples)[[1]],
#             analz_samples(mu=mu0,theta=theta0,v=0.5,mysample = samples)[[1]], ncol=2) 


## Gráficos para theta0 = 9

theta0 <- 9
samples <- do.call(as.matrix, G.samples(N=10000,n=n0,theta=theta0,mu=mu0))# Amostras geradas com os valores de n, N, theta e mu.
grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.1,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.1,mysample = samples)[[1]], ncol=2) 
grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.2,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.2,mysample = samples)[[1]], ncol=2) 
grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.3,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.3,mysample = samples)[[1]], ncol=2) 
grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.4,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.4,mysample = samples)[[1]], ncol=2) 
#grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.5,mysample = samples)[[1]],
#             analz_samples(mu=mu0,theta=theta0,v=0.5,mysample = samples)[[1]], ncol=2) 


grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.1,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.1,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=-0.2,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.2,mysample = samples)[[1]], 
             analz_samples(mu=mu0,theta=theta0,v=-0.3,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.3,mysample = samples)[[1]], 
             analz_samples(mu=mu0,theta=theta0,v=-0.4,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.4,mysample = samples)[[1]], ncol=4) 
#grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.5,mysample = samples)[[1]],
#             analz_samples(mu=mu0,theta=theta0,v=0.5,mysample = samples)[[1]], ncol=2) 




## Gráficos para theta0 = 100

theta0 <- 100
samples <- do.call(as.matrix, G.samples(N=10000,n=n0,theta=theta0,mu=mu0))# Amostras geradas com os valores de n, N, theta e mu.
grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.1,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.1,mysample = samples)[[1]], ncol=2) 
grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.2,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.2,mysample = samples)[[1]], ncol=2) 
grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.3,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.3,mysample = samples)[[1]], ncol=2) 
grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.4,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.4,mysample = samples)[[1]], ncol=2) 
#grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.5,mysample = samples)[[1]],
#             analz_samples(mu=mu0,theta=theta0,v=0.5,mysample = samples)[[1]], ncol=2) 

grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.1,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.1,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=-0.2,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.2,mysample = samples)[[1]], 
             analz_samples(mu=mu0,theta=theta0,v=-0.3,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.3,mysample = samples)[[1]], 
             analz_samples(mu=mu0,theta=theta0,v=-0.4,mysample = samples)[[1]],
             analz_samples(mu=mu0,theta=theta0,v=0.4,mysample = samples)[[1]], ncol=4) 
#grid.arrange(analz_samples(mu=mu0,theta=theta0,v=-0.5,mysample = samples)[[1]],
#             analz_samples(mu=mu0,theta=theta0,v=0.5,mysample = samples)[[1]], ncol=2) 






############### Analisar o n ################

analz_samples2 <- function(mu,theta,v,u=-v,n=ncol(samples),N=10000,mysample){
  g_theta <- function(theta0){(theta0+1)^(u-v)*beta(v+1,theta0-v+1)^u*beta(u+1,theta0-u+1)^(-v)} ## We want to have the inverse of this function 
  g_theta_inv<-inverse(g_theta,lower=0,upper=10000) # Inverse of g_theta
  mu_uv <- (rowMeans(mysample^v)^u)/(rowMeans(mysample^u)^v) # T_v^u/T_u^v
  lim_inf <- ((gamma(v)*v)^u/((gamma(u)*u)^v))*((-gamma(-v)*v)^u/((-gamma(-u)*u)^v))## Limite de g_theta quando theta tende a zero
  limsup <- (1000000000+1)^(u-v)*beta(v+1,1000000000-v+1)^u*beta(u+1,1000000000-u+1)^(-v) ## Limite superior (assíntota)
  prop_rejec <-   sum(ifelse(mu_uv>limsup,1,0))/N
  ### Gráfico com g(theta) e os valores da estatística Tn = T_v^u/T_u^v
  p3 <- ggplot(data = data.frame(x = 0,z=limsup, w = mu_uv), mapping = aes(x = x,w)) +
    stat_function(fun = g_theta) + xlim(0,500) + 
    #annotate("text", x=500, y=g_theta(0.64), label= paste("u = ", u, ", v = ", v),size = 4, colour = "red",fontface = "bold") +
    labs(title = bquote(paste("n = ", .(n),", Proporção rejeitada = ",.(prop_rejec))),x=expression(theta), y = bquote(paste("g(",theta,")"))) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5))+
    geom_hline(yintercept = limsup, linetype = 5,size=0.5)+
    geom_point(colour = "black", size = 0.3)
  return(list(p3,prop_rejec))
}
## Analisar mudando o n e fixando theta

theta0=1/9
n0 <- 20
samples <- do.call(as.matrix, G.samples(N=10000,n=n0,theta=theta0,mu=mu0))# Amostras geradas com os valores de n, N, theta e mu.
p_n1 <- analz_samples2(mu=mu0,theta=theta0,v=0.4,mysample = samples)[[1]]


n0 <- 50
samples <- do.call(as.matrix, G.samples(N=10000,n=n0,theta=theta0,mu=mu0))# Amostras geradas com os valores de n, N, theta e mu.
p_n2 <- analz_samples2(mu=mu0,theta=theta0,v=0.4,mysample = samples)[[1]]

n0 <- 100
samples <- do.call(as.matrix, G.samples(N=10000,n=n0,theta=theta0,mu=mu0))# Amostras geradas com os valores de n, N, theta e mu.
p_n3 <- analz_samples2(mu=mu0,theta=theta0,v=0.4,mysample = samples)[[1]]

n0 <- 500
samples <- do.call(as.matrix, G.samples(N=10000,n=n0,theta=theta0,mu=mu0))# Amostras geradas com os valores de n, N, theta e mu.
p_n4 <- analz_samples2(mu=mu0,theta=theta0,v=0.4,mysample = samples)[[1]]


grid.arrange(p_n1,p_n3,p_n4,ncol = 3)

############### Função para gerar as quantidades teóricas ###############
quantity_t <- function(mu0,theta0,v,u=-v,d0){
  ## d0 é a aproximação numérica inicial
  g_theta <- function(theta0){(theta0+1)^(u-v)*beta(v+1,theta0-v+1)^u*beta(u+1,theta0-u+1)^(-v)}
  g_theta_inv<-inverse(g_theta,lower=0,upper=10000)
  ev <- (mu0*theta0)^v*(theta0+1)*beta(v+1,theta0+1-v) # Expected value of X^v
  eu <- (mu0*theta0)^u*(theta0+1)*beta(u+1,theta0+1-u) # Expected value of X^u
  sigv <- (mu0*theta0)^(2*v)*(theta0+1)*(beta(2*v+1,theta0+1-2*v)-(theta0+1)*(beta(v+1,theta0+1-v)^2)) # Variance of X^v
  sigu <- (mu0*theta0)^(2*u)*(theta0+1)*(beta(2*u+1,theta0+1-2*u)-(theta0+1)*(beta(u+1,theta0+1-u)^2)) # Variance of X^u
  siguv <- ((mu0*theta0)^(u+v))*(theta0+1)*(beta(u+v+1,theta0+1-u-v)-(theta0+1)*beta(u+1,theta0+1-u)*beta(v+1,theta0+1-v)) # Covariance (X^v,X^u)
  p1 <- (u^2)*sigv*ev^(2*u-2)*eu^(-2*v)
  p2 <- u*v*ev^(2*u-1)*eu^(-2*v-1)*siguv
  p3 <- (v^2)*sigu*ev^(2*u)*eu^(-2*v-2)
  gamma2 <- p1-2*p2+p3  # Variance of g_1
  EspTn <- (ev^u)/(eu^v) # g_1 applied in (ev, eu)
  inv_esp <- g_theta_inv(EspTn)
  der_inve <- grad(g_theta_inv,EspTn, method.args = list(eps=1e-12,d=d0))
  return(c(gamma2,EspTn,inv_esp,der_inve))
}




############### Função paa calulcar a prob assintótica ###############

Fun2 <- function(theta,mu,n,v,u=-v){
  ## gerar quantidades teóricas
  ev <- (mu*theta)^v*(theta+1)*beta(v+1,theta+1-v)
  eu <- (mu*theta)^u*(theta+1)*beta(u+1,theta+1-u)
  sigv <- (mu*theta)^(2*v)*(theta+1)*(beta(2*v+1,theta+1-2*v)-(theta+1)*(beta(v+1,theta+1-v)^2))
  sigu <- (mu*theta)^(2*u)*(theta+1)*(beta(2*u+1,theta+1-2*u)-(theta+1)*(beta(u+1,theta+1-u)^2))
  siguv <- ((mu*theta)^(u+v))*(theta+1)*(beta(u+v+1,theta+1-u-v)-(theta+1)*beta(u+1,theta+1-u)*beta(v+1,theta+1-v))
  p1 <- (u^2)*sigv*ev^(2*u-2)*eu^(-2*v)
  p2 <- u*v*ev^(2*u-1)*eu^(-2*v-1)*siguv
  p3 <- (v^2)*sigu*ev^(2*u)*eu^(-2*v-2)
  gamma2 <- p1-2*p2+p3 ; gamma2
  EspTn <- (ev^u)/(eu^v);EspTn
  lsup <- (10000+1)^(u-v)*beta(v+1,10000-v+1)^u*beta(u+1,10000-u+1)^(-v)
  xlim1 <- (1.88*sqrt(gamma2)/(lsup-EspTn))^2
  xlim2 <- (10*sqrt(gamma2)/(lsup-EspTn))^2
  ### Caluclar a probablidade
  probM <- pnorm(sqrt(n)*(lsup-EspTn)/sqrt(gamma2))
  return(c(probM,xlim1,xlim2))
}

Fun2(theta=1/9,mu=log(3),n=1000,v=0.1)
round(Fun2(theta=9,mu=log(3),n=1000,v=0.4),0)


## Gráfico para n e a probabilidade de rejeitar
theta00 <- 100
xlim2 <- Fun2(theta = theta00,mu=10,n=1000,v=0.4)[2]
xlim3 <- ifelse(xlim2<200,200,xlim2)
aux<-seq(0,xlim3,length=1000)
df1<-sapply(c(0.1,0.2,0.3,0.4),function(i){
  sapply(aux,Fun2,theta=theta00,mu=10,v=i,u= -i)[1,]
})

df2<-gather(df1 %>% as.data.frame())

df2 %>%
  mutate(key=case_when(
    key=="V1"~0.1,
    key=="V2"~0.2,
    key=="V3"~0.3,
    TRUE ~ 0.4)
  ) %>% 
  cbind(x=rep(aux,4)) %>% 
  ggplot(aes(x=x,y=value)) + 
  geom_line() + 
  #facet_wrap(~key, labeller = label_bquote(paste("v = ",.(key)))) +
  facet_rep_wrap(~key, labeller = label_bquote(paste("v = ",.(key))), scales = "fixed", repeat.tick.labels = TRUE)+
  theme_bw()+
  labs(x=expression(n), y = expression(Phi(sqrt(n)*(l[uv]-mu[uv])/kappa))) +
  geom_hline(yintercept = 0.97, linetype = 5,size=0.5)
  



############### Estimar theta e q ###############


f.MMF <- function(mu,theta,v,u=-v,n=ncol(samples),N=10000,mysample, d0){
  mu_hat <- sapply(mysample, mean) # theta estimado com base na amostra selecionada
  g_theta <- function(theta0){(theta0+1)^(u-v)*beta(v+1,theta0-v+1)^u*beta(u+1,theta0-u+1)^(-v)} ## We want to have the inverse of this function 
  g_theta_inv<-inverse(g_theta,lower=0,upper=1000) # Inverse of g_theta
  call_quant <- quantity_t(mu,theta,v,u=-v, d0=d0) ## chamar a função quantity_t
  lim_inf <- liminf(v=v,u=u)
  lim_sup <- limsup(v=v,u=u)
  mu_uv <- (rowMeans(mysample^v)^u)/(rowMeans(mysample^u)^v) # T_v^u/T_u^v
  ifelse(v>0,samp_cond <- mu_uv[mu_uv > lim_inf & mu_uv < lim_sup],
         samp_cond <- mu_uv[mu_uv < lim_inf & mu_uv > lim_sup])# Selecionando a amostra
  theta_hat <- sapply(samp_cond, g_theta_inv) # theta estimado com base na amostra selecionada
  kappa2 <- call_quant[1]*(call_quant[4])^2 # variância obtida pelo método delta para a inversa da função
  theta_hat_pad <- (sqrt(n)*(theta_hat-theta))/sqrt(kappa2) # padronizando o theta estimado
  q_hat = (3+theta_hat)/(2+theta_hat) ## Estimativas de q
  var_qhat <-(call_quant[3]+2)^(-4)*kappa2 ## Variância assintótica de q
  qreal <- (3+theta)/(2+theta) ## Valor verdadeiro de q
  q_hat_pad <- (sqrt(n)*(q_hat-qreal))/sqrt(var_qhat) # padronizando o q estimado
  d_q_theta <- data.frame(theta_hat,theta_hat_pad,q_hat,q_hat_pad)
  mylist<-list(d_q_theta,theta_hat,theta_hat_pad,mu_hat,q_hat, q_hat,kappa2,lim_sup)
  return(mylist)
}







############### Exemplos ###############

############### Definir os valores iniciais ###############
v0 <- 0.45
u0 <- -v
mu0 <- log(3)
theta0 <- 80
n0 <- 1000

### Exemplo para a função g(theta)

grid.arrange(g_theta_func(v=0.1)[[1]], g_theta_func(v=-0.1)[[1]], ncol=2) ## COnsiderando um valor particular de u e v
grid.arrange(g_theta_func(v=0.2)[[1]], g_theta_func(v=-0.2)[[1]], ncol=2) ## COnsiderando um valor particular de u e v
grid.arrange(g_theta_func(v=0.3)[[1]], g_theta_func(v=-0.3)[[1]], ncol=2) ## COnsiderando um valor particular de u e v
grid.arrange(g_theta_func(v=0.4)[[1]], g_theta_func(v=-0.4)[[1]], ncol=2) ## COnsiderando um valor particular de u e v
grid.arrange(g_theta_func(v=0.5)[[1]], g_theta_func(v=-0.5)[[1]], ncol=2) ## COnsiderando um valor particular de u e v

grid.arrange(g_theta_func(v=0.1)[[2]], g_theta_func(v=-0.1)[[3]], ncol=2) ## Curvas para diferentes valores de u e v




## Gerar a amostra

n0 = 1000
theta0=1/9
mu0=log(3)
samples <- do.call(as.matrix, G.samples(N=10000,n=n0,theta=theta0,mu=mu0))# Amostras geradas com os valores de n, N, theta e mu.

#### Exemplo para a função que gera os valores teóricos

quantity_t(mu0=mu0,theta0=theta0,v=v0,d0=1e-10)


## Curvas para estimativas de theta e q

par(mfrow=c(1,2))
f.MMF(mu=mu0,theta=theta0,v=v0,mysample = samples,d0 = 1e-10)

