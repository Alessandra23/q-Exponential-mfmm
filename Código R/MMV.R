## Estimação q-Exponencial via Método de Máxima Verossimilhança

#### Pacotes #### 

library(tidyverse)
library(ggplot2)
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


######## Estimação ##########

theta0 <- 1/9
mu0 <- log(3)
n0 <- 10000
q0 <- (theta0+3)/(theta0+2)

samples <- do.call(as.matrix, G.samples(N=10000,n=n0,theta=theta0,mu=mu0)) # Amostras geradas com os valores de n, N, theta e mu.

## Log verossimilhança
log_qexp <- function(delta,x){
  mu <- delta[1]
  theta <- delta[2]
  n <- length(x)
  return(-(n*log((theta+1)/(theta*mu))-(theta+2)*sum(log(1+x/(mu*theta)))))
}


## log likelihood

## Score function
score_qexp <- function(delta){
  mu <- delta[1]
  theta <- delta[2]
  score_vec <- numeric(2)
  score_vec[1] <- -(n/mu)+((theta+2)/mu^2*theta)*sum(x*(1+(x/(mu*theta)))^(-1))
  score_vec[2] <- -(n/(theta*(theta+1)))-sum(log(1+(x/(mu*theta)))) + ((theta+2)/(theta^2*mu))*sum(x*(1+(x/(mu*theta)))^(-1))
  return(score_vec)
}

## Hessian Matrix
hess_qexp <- function(delta){
  mu <- delta[1]
  theta <- delta[2]
  H <- matrix(0,2,2)
  H[1,1] <- (n/mu^2)-(2*(theta+2)/mu^3*theta)*sum(x*(1+(x/(mu*theta)))^(-1))+((theta+2)/(mu^4*theta^2))*sum(x^2*(1+(x/(mu*theta)))^(-2))
  H[2,2] <- -(n*(2*theta+1)/(theta^2*(theta+1)^2)) -(4/(mu*theta^3))*sum(x*(1+(x/(mu*theta)))^(-1)) + ((theta+2)/(mu^2*theta^4))*sum(x^2*(1+(x/(mu*theta)))^(-2))
  H[1,2] <- -(2/(mu^2*theta^2))*sum(x*(1+(x/(mu*theta)))^(-1)) + ((theta+2)/mu^3*theta^3)*sum(x^2*(1+(x/(mu*theta)))^(-2))
  H[2,1] <- H[1,2]
  return(H)
}



## Função para obter um par de estimativas
f1 <- function(samples,i){
  y <- samples[i,]
  mu_in <- mean(y)
  theta_in <- mean(y^0.1)/(mean(y)^0.1)
  tt <- optim(par = c(mu_in,theta_in), log_qexp, x = y)
  return(tt$par)
}

## Aplicando a função f1 para cada amostra (cada linha das 10000 réplicas de Monte Carlo)
est_MV <- sapply(1:nrow(samples),f1,samples=samples) %>% t()

est_MV_mu <- est_MV[,1]
est_MV_theta <- est_MV[,2]
est_MV_q <- (3+est_MV_theta)/(2+est_MV_theta)


# Expectited Fisher Information
## Without n
K_mu_theta <- function(delta){
  mu <- delta[1]
  theta <- delta[2]
  KF <- matrix(0,2,2)
  KF[1,1] <- ((theta+2)^2)/(mu^2*(theta^2+5*theta+6))
  KF[2,2] <- (theta^2+theta+2)/(theta^2*(theta+1)^2*(theta^2+5*theta+6))
  KF[1,2] <- 2/(mu*theta*(theta^2+5*theta+6))
  KF[2,1] <- KF[1,2]
  return(KF)
}


## Asymptotic variance 
varmu <- solve(K_mu_theta(c(mu0,theta0)))[1,1] ## Of mu
vartheta <- solve(K_mu_theta(c(mu0,theta0)))[2,2] ## Of theta

####### Histograms ####

est_MV_mu %>% hist()
est_MV_theta %>% hist()
est_MV_q %>% hist()
hist(est_MV_q,freq = FALSE)


## Plots standart

mu_pad_mv <- sqrt(n0)*(est_MV_mu-mu0)/sqrt(varmu)
hist(mu_pad_mv, prob=TRUE)
curve(dnorm,add = T, col = "blue")

theta_pad_mv <- sqrt(n0)*(est_MV_theta-theta0)/sqrt(vartheta)
hist(theta_pad_mv, prob=TRUE)
curve(dnorm,add = T, col = "blue")

q_pad_mv <- sqrt(n0)*(est_MV_q-q0)/sqrt(vartheta*((theta0+2)^(-4)))
hist(q_pad_mv,prob = TRUE)
curve(dnorm,add = T, col = "blue")


par(mfrow = c(2,2))
est_MV_mu %>% hist()
est_MV_theta %>% hist()
est_MV_q %>% hist()
hist(q_pad_mv,prob = TRUE)
curve(dnorm,add = T, col = "blue")

#######

I_Fisher <- function(n,delta){
  mu <- delta[1]
  theta <- delta[2]
  IF <- matrix(0,2,2)
  IF[1,1] <- (n/mu^2)-((n/(mu^3*theta))*((theta+1)*(theta+2))/(theta^2+5*theta+6))
  IF[2,2] <- n*((2*theta+1)/(theta^2*(theta+1)^2)+ (4*(theta+1)/(theta^2*(theta^2+3*theta+2)))-((theta+1)*(theta+2)/(mu*theta^3*(theta^2+5*theta+6))))
  IF[1,2] <- n*((2*(theta+1)/(mu*theta*(theta^2+3*theta+2)))-((theta+1)*(theta+2)/(mu^2*theta^2*(theta^2+5*theta+6))))
  IF[2,1] <- IF[1,2]
  return(IF)
}

###### Histograms #######

#######theta padronizado #######

## data frame com todos os valores
df_est_mv <- data.frame(est_MV_mu,est_MV_theta,est_MV_q, mu_pad_mv,theta_pad_mv, q_pad_mv)

## Individuais
df_est_mv %>% ggplot() +
  geom_density(aes(x=est_MV_mu)) +
  labs( x = expression(hat(mu)),y = "Densidade")

df_est_mv %>% ggplot() +
  geom_density(aes(x=est_MV_theta)) +
  labs( x = expression(hat(theta)),y = "Densidade")


df_est_mv %>% ggplot() +
  geom_density(aes(x=est_MV_q)) +
  labs( x = expression(hat(q)),y = "Densidade")


## Padronizados

df_est_mv %>% ggplot() +
  geom_density(aes(x=theta_pad_mv,linetype = "Densidade padronizada")) +
  stat_function(fun = dnorm,n=101, args = list(mean = 0, sd = 1),aes(linetype="Normal Padrao")) +
  scale_linetype_manual(name=" ",values=c("solid","dashed"))+
  labs( x = expression(hat(theta)[Pad]),y = "Densidade")


df_est_mv %>% ggplot() +
  geom_density(aes(x=theta_pad_mv,linetype = "Densidade padronizada")) +
  stat_function(fun = dnorm,n=101, args = list(mean = 0, sd = 1),aes(linetype="Normal Padrao")) +
  scale_linetype_manual(name=" ",values=c("solid","dashed"))+xlim(-5,5)+
  labs( x = expression(hat(theta)[Pad]),y = "Densidade")



## To theta

plot_theta <- function(size,mu,theta){
  mysamp <- do.call(as.matrix, G.samples(N=10000,n=size,theta=theta,mu=mu))# Amostras geradas com os valores de n, N, theta e mu.
  df1 <- sapply(1:nrow(mysamp),f1,samples=mysamp) %>% t()
  return(df1[,2]) # retornar a segunda coluna da matrix pois tem as estimativas de theta
}

## generate the plots to different values of theta standart
p_theta <- lapply(c(1/9,1,9), function(theta,size){
              n <- size
              y <- sapply(n,plot_theta,theta = theta,mu=mu0)
              
              Lengths <- y %>%
                         map(length) %>%
                         as.numeric()
              
              typeLine <- c(rep(n[1],Lengths[1]),rep(n[2],Lengths[2]),rep(n[3],Lengths[3]))
              
              df <- y %>%
                    unlist() %>%
                    as.numeric %>% 
                    cbind(typeLine) %>% 
                    as.data.frame() %>% 
                    cbind(rep(theta,nrow(.))) %>% 
                    magrittr::set_colnames(c("x","type","T0"))
                    return(df)
           }, size=c(20,30,50)) %>% 
          bind_rows()%>% 
          ggplot(aes(x=x))+
          geom_density(aes(linetype=factor(type)))+
          stat_function(fun = dnorm,n=101, args = list(mean = 0, sd = 1),aes(linetype="Normal"))+
          scale_linetype_manual(name="n",values=c("dashed","dotdash", "dotted","solid"))+xlim(c(-5,5))+
          facet_rep_wrap(~T0, labeller = label_bquote(paste(theta," = ",.(T0))), scales = "fixed", repeat.tick.labels = TRUE)+
          theme_bw()+
          labs(x = expression(hat(theta)[Pad]), y = "Densidade")
      
p_theta


## To theta

plot_theta2 <- function(size,mu,theta){
  mysamp <- do.call(as.matrix, G.samples(N=10000,n=size,theta=theta,mu=mu))# Amostras geradas com os valores de n, N, theta e mu.
  df1 <- sapply(1:nrow(mysamp),f1,samples=mysamp) %>% t()
  return(df1[,2]) # retornar a segunda coluna da matrix pois tem as estimativas de theta
}

## generate the plots to different values of theta without standart
p_theta2 <- lapply(c(round(1/9,2),1,9), function(theta,size){
  n <- size
  y <- sapply(n,plot_theta2,theta = theta,mu=mu0)
  
  Lengths <- y %>%
    map(length) %>%
    as.numeric()
  
  typeLine <- c(rep(n[1],Lengths[1]),rep(n[2],Lengths[2]),rep(n[3],Lengths[3]),rep(n[4],Lengths[4]))
  
  df <- y %>%
    unlist() %>%
    as.numeric %>% 
    cbind(typeLine) %>% 
    as.data.frame() %>% 
    cbind(rep(theta,nrow(.))) %>% 
    magrittr::set_colnames(c("x","type","T0"))
  return(df)
}, size=c(20,30,50,500)) %>% 
  bind_rows()%>% 
  ggplot(aes(x=x))+
  geom_density(aes(linetype=factor(type)))+
  scale_linetype_manual(name="n",values=c("dashed","dotdash", "dotted","solid"))+ xlim(0,100)+
  facet_rep_wrap(~T0, labeller = label_bquote(paste(theta," = ",.(T0))), scales = "fixed", repeat.tick.labels = TRUE)+
  theme_bw()+
  labs(x = expression(hat(theta)[Pad]), y = "Densidade")

p_theta2




## To q

plot_q2 <- function(size,mu,theta){
  mysamp <- do.call(as.matrix, G.samples(N=10000,n=size,theta=theta,mu=mu))# Amostras geradas com os valores de n, N, theta e mu.
  df1 <- sapply(1:nrow(mysamp),f1,samples=mysamp) %>% t()
  qest <- (3+df1[,2])/(2+df1[,2]) 
  return(qest) # retornar a segunda coluna da matrix pois tem as estimativas de theta
}

## generate the plots to different values of theta without standart
p_q2 <- lapply(c(1.09,1.33,1.47), function(theta,size){
  n <- size
  y <- sapply(n,plot_q2,theta = theta,mu=mu0)
  
  Lengths <- y %>%
    map(length) %>%
    as.numeric()
  
  typeLine <- c(rep(n[1],Lengths[1]),rep(n[2],Lengths[2]),rep(n[3],Lengths[3]),rep(n[4],Lengths[4]))
  
  df <- y %>%
    unlist() %>%
    as.numeric %>% 
    cbind(typeLine) %>% 
    as.data.frame() %>% 
    cbind(rep(theta,nrow(.))) %>% 
    magrittr::set_colnames(c("x","type","q0"))
  return(df)
}, size=c(20,30,50,500)) %>% 
  bind_rows()%>% 
  ggplot(aes(x=x))+
  geom_density(aes(linetype=factor(type)))+
  scale_linetype_manual(name="n",values=c("dashed","dotdash", "dotted","solid"))+ xlim(1,1.5)+
  facet_rep_wrap(~q0, labeller = label_bquote(paste("q = ",.(q0))), scales = "fixed", repeat.tick.labels = TRUE)+
  theme_bw()+
  labs(x = expression(hat(theta)[Pad]), y = "Densidade")

p_q2


