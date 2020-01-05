## Insurance data

library(insuranceData)
library(MASS)
library(fitdistrplus)
library(GoFKernel) ## Calcular a inversa
library(ggplot2)

head("AutoBi")
data(AutoBi)
dat <- AutoBi$LOSS
str(dat)
summary(dat)
hist(dat)
boxplot(dat)
str(AutoBi)
par(mfrow=c(1,1))


## Log verossimilhança
log_qexp <- function(delta,x){
  mu <- delta[1]
  theta <- delta[2]
  n <- length(x)
  return(-(n*log((theta+1)/(theta*mu))-(theta+2)*sum(log(1+x/(mu*theta)))))
}


### Estimando via momentos fracionários

v=0.1
u<- -v
g_theta <- function(theta0){(theta0+1)^(u-v)*beta(v+1,theta0-v+1)^u*beta(u+1,theta0-u+1)^(-v)} ## We want to have the inverse of this function 
g_theta_inv<-inverse(g_theta,lower=0,upper=1000) # Inverse of g_theta
mu_uv <- (mean(dat^v))^u/(mean(dat^u))^v # T_v^u/T_u^v
theta_hat_mf <- g_theta_inv(mu_uv)
mu_hat_mf <- mean(dat)
c(mu_hat_mf,theta_hat_mf)

hist(dat, prob = T, breaks = 60)
curve(dtsal(x, shape = theta_hat_mf+1, scale = mu_hat_mf*theta_hat_mf), add = T, col = "blue")


## Estimando via máxima verossimilhança


mu_in <- mean(dat)
theta_in <- mean(dat^0.1)/(mean(dat)^0.1)
tt <- optim(par = c(mu_in,theta_in), log_qexp, x = dat)
tt$par


hist(dat, prob = T,  breaks = 60, main = " ", xlab = "LOSS", ylab = "Densidade")
curve(dtsal(x,shape = tt$par[2]+1, scale = tt$par[1]*tt$par[2]),add = T, col = "blue")
curve(dtsal(x, shape = theta_hat_mf+1, scale = mu_hat_mf*theta_hat_mf), add = T, col = "red")
legend("right", legend=c("MMFM", "MMV"),
       lty=1, col=c("red","blue"), bty="n")


curve(dexp(x,1/mean(dat)), add = T, col = "green")


### Estimando via log-cumulantes

k1 <- sum(log(dat))/length(dat);k1
k2 <- (sum(log(dat)-k1)^2)/length(dat);k2
theta_hat_lc <- Inv(k2-trigamma(1))-1 



denst <- function(x) (theta + 1)/(theta*mu)*(1 + x/(theta*mu))^(-theta- 2)

d1 <- dtsal(x,shape = theta+1,scale = mu*theta)

xval = seq(0,15,.10)
y2 = dtsal(xval,shape = theta+1,scale = mu*theta)
y3 = dexp(xval,1)

data

p1 <- data.frame(xval,y2,y3) %>% 
  gather(cond,yval,-xval)%>% 
  ggplot(aes(x=xval, y=yval)) +
  geom_line(aes(linetype=cond),size = 1) +
  scale_linetype_discrete("", labels = c("q-Exponencial", "Exponencial"))+
  ylab("f(x)") + xlab("x")  +   theme_bw()

p1

#####################################

dados <- data.frame(x=dat)
ggplot(dados, aes(x=x)) +
  geom_histogram(bins = 50, aes(y = ..density..),fill="white",color="black")+
  stat_function(fun = dtsal, args = list(shape = tt$par[2]+1, scale = tt$par[1]*tt$par[2]),show.legend=T, aes(linetype="solid"))+ ## ajuste por MV
  stat_function(fun = dtsal, args = list(shape = theta_hat_mf+1, scale = mu_hat_mf*theta_hat_mf),show.legend=T,aes(linetype="dotted"))+ ## ajuste por MMFM
  scale_linetype_manual("Método",values = c("solid", "dotted"),
                        labels = c("MMV", "MMFM"))+
  ylab("Densidade") + xlab("LOSS") +
  coord_cartesian(ylim = c(0,0.042))+
  theme_bw()

#####################################




## Ajustar usando essa
fitdistr()


fit_w  <- fitdist(dat, dtsal,shape = tt$par[2]+1, scale = tt$par[1]*tt$par[2])
fit_g  <- fitdist(dat, "gamma")
fit_ln <- fitdist(dat, "lnorm")
summary(fit_ln)
par(mfrow=c(2,2))
plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fit_w, fit_g, fit_ln), legendtext = plot.legend)
cdfcomp (list(fit_w, fit_g, fit_ln), legendtext = plot.legend)
qqcomp  (list(fit_w, fit_g, fit_ln), legendtext = plot.legend)
ppcomp  (list(fit_w, fit_g, fit_ln), legendtext = plot.legend)


fit_ln <- fitdist(dat, "lnorm")
cdfcomp(fit_ln, xlogscale = TRUE, ylogscale = TRUE)


library(actuar)

fit_ll <- fitdist(dat, "llogis", start = list(shape = 1, scale = 500))
fit_P  <- fitdist(dat, "pareto", start = list(shape = 1, scale = 500))
fit_B  <- fitdist(dat, "burr",   start = list(shape1 = 0.3, shape2 = 1, rate = 1))
cdfcomp(list(fit_ln, fit_ll, fit_P, fit_B), xlogscale = TRUE, ylogscale = TRUE,
        legendtext = c("lognormal", "loglogistic", "Pareto", "Burr"), lwd=2)
