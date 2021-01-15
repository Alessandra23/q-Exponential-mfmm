#' g(theta) plots 
#' 
#'
#'

source("qexp_functions.R")
source("qexp_generate_samples.R")
source("mfmm/gtheta_function.R")

library(gridExtra)
theme_set(theme_bw())

# values of v>0
v1 <- seq(0.1,0.5,0.1)
theta <- c(0:100)
df.v1 <- outer(theta, v1,g.theta) 
df.v1 <- data.frame(df.v1, x = rep(0:100,length(v1)))

p.v1 <- ggplot(melt(df.v1,id.vars='x'), aes(x,value,group=variable))+
          geom_line(aes(linetype = variable)) +
          scale_linetype_discrete(labels=v1, name="v")+
          labs(x=expression(theta), y = bquote(paste("g(",theta,")")))+
          theme(legend.position=c(0.85,0.2))


p.v1


# values of v<0
v2 <- seq(-0.5,-0.1,0.1)
theta <- c(0:100)
df.v2 <- outer(theta, v2,g.theta) 
df.v2 <- data.frame(df.v2, x = rep(0:100,length(v2)))

p.v2 <- ggplot(melt(df.v2,id.vars='x'), aes(x,value,group=variable))+
  geom_line(aes(linetype = variable)) +
  scale_linetype_discrete(labels=v2, name="v")+
  labs(x=expression(theta), y = bquote(paste("g(",theta,")")))+
  theme(legend.position=c(0.85,0.8))


p.v2

grid.arrange(p.v1,p.v2, nrow = 1)


# v = -0.5:0.5

v3 <- c(v1,v2)
theta <- c(0:100)
df.v3 <- outer(theta, v3,g.theta) 
df.v3 <- data.frame(df.v3, x = rep(0:100,length(v3)))

p.v3 <- ggplot(melt(df.v3,id.vars='x'), aes(x,value,group=variable))+
  geom_line(aes(linetype = variable)) +
  scale_linetype_discrete(labels=v3, name="v")+
  labs(x=expression(theta), y = bquote(paste("g(",theta,")")))


p.v3



