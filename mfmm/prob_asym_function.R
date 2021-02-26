library(devtools)
devtools::install_github("Alessandra23/q-Exponential-mfmm/qExponential")
library(qExponential)


library(tidyverse)
library(ggplot2)
theme_set(theme_bw())


# Graph for n and the probability of rejecting


plot.reject <- function(n, mu, theta, v){
  lim <- prob.asym(n = n, mu = mu , theta = theta, v=v)[2]
  lim <- ifelse(lim < 200,200,lim)
  lim <- seq(0,lim,length=1000)
  
  
  mat.pr <- sapply(c(0.1,0.2,0.3,0.4),function(i){
    sapply(lim,prob.asym, mu = log(3), theta = 1/9 ,v=i)[1,]
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
    geom_hline(yintercept = 0.97, linetype = 5,size=0.5)
  
  return(p.pr)
  
}

# theta = 1/9 

lim <- prob.asym(n = 1000, mu = log(3), theta = 1/9, v=0.4)[2]
lim <- ifelse(lim < 200,200,lim)
lim <- seq(0,lim,length=1000)


mat.pr <- sapply(c(0.1,0.2,0.3,0.4),function(i){
          sapply(lim,prob.asym,mu = log(3), theta = 1/9 ,v=i)[1,]
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
          geom_hline(yintercept = 0.97, linetype = 5,size=0.5)

p.pr



# theta = 1

lim <- unlist(prob.asym(n = 1000, mu = log(3), theta = 1, v=0.4)[2])
lim <- seq(0,lim,length=1000)


mat.pr <- sapply(c(0.1,0.2,0.3,0.4),function(i){
  sapply(lim,prob.asym,mu = log(3), theta = 1,v=i)[1,]
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
  geom_hline(yintercept = 0.97, linetype = 5,size=0.5)

p.pr


# theta = 9

values <- list(mu = log(3), theta = 9)

lim <- unlist(prob.asym(n = 1000, mu = log(3), theta = 9, v=0.4)[2])
lim <- seq(0,lim,length=1000)


mat.pr <- sapply(c(0.1,0.2,0.3,0.4),function(i){
  sapply(lim,prob.asym,mu = log(3), theta = 9,v=i)[1,]
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
  geom_hline(yintercept = 0.97, linetype = 5,size=0.5)

p.pr



# theta = 100

values <- list(mu = log(3), theta = 100)

lim <- unlist(prob.asym(n = 1000, mu = log(3), theta = 100, v=0.4)[2])
lim <- seq(0,lim,length=1000)


mat.pr <- sapply(c(0.1,0.2,0.3,0.4),function(i){
  sapply(lim,prob.asym, mu = log(3), theta = 100,v=i)[1,]
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
  geom_hline(yintercept = 0.97, linetype = 5,size=0.5)

p.pr




