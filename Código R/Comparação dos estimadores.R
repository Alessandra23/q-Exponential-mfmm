############ Comparação dos estimadores #########

#### Gerar tabela 

theta0 <- 1/9
mu0 <- log(3)
n0 <- 10000
q0 <- (theta0+3)/(theta0+2)




table_comp <- function(n0,mu0,theta0){
  
  samples <- do.call(as.matrix, G.samples(N=10000,n=n0,theta=theta0,mu=mu0)) # Amostras geradas com os valores de n, N, theta e mu.
  
  ## Por MMF
  
  est_MMF <- f.MMF(mu = mu0,theta = theta0,v=0.1,u=-0.1,n=ncol(samples),N=10000,mysample = samples, d0 = 1e-10)
  est_MMF_mu <- est_MMF[[4]] ## Estimativas por MV de mu
  est_MMF_theta <- est_MMF[[5]] ## Estimativas por MV de theta
  mean_MMF_mu <- mean(est_MMF_mu) ## media mu
  var_MMF_mu <- var(est_MMF_mu) ## var mu
  mean_MMF_theta <- mean(est_MMF_theta) ## media theta
  var_MMF_theta <- var(est_MMF_theta) ## var theta
  
  ## Por MV
  est_MV <- sapply(1:nrow(samples),f1,samples=samples) %>% t()
  est_MV_mu <- est_MV[,1] ## Estimativas por MV de mu
  est_MV_theta <- est_MV[,2] ## Estimativas por MV de theta
  mean_MV_mu <- mean(est_MV_mu) ## media mu
  var_MV_mu <- var(est_MV_mu) ## var mu
  mean_MV_theta <- mean(est_MV_theta) ## media theta
  var_MV_theta <- var(est_MV_theta) ## var theta
  
  df_comp <- data.frame(mean_MMF_mu,mean_MMF_theta,var_MMF_mu,var_MMF_theta,
                        mean_MV_mu, mean_MV_theta,var_MV_mu,var_MV_theta)
  return(df_comp)
}


round(table_comp(n0 = 500, mu0 = 1/10, theta0 = 1/9),4)
round(table_comp(n0 = 500, mu0 = 1/10, theta0 = 9),4)
round(table_comp(n0 = 500, mu0 = 10, theta0 = 1/9),4)
round(table_comp(n0 = 500, mu0 = 10, theta0 = 9),4)
round(table_comp(n0 = 500, mu0 = log(3), theta0 = 1),4)


############  Comparar as variâncias  ############


comp_var <- function(mu,theta,v,d0){
  call_quant <- quantity_t(mu,theta,v,u=-v, d0=d0) ## chamar a função quantity_t
  kappa2 <- call_quant[1]*(call_quant[4])^2 # variância obtida pelo método delta para a inversa
  vartheta <- solve(K_mu_theta(c(mu,theta)))[2,2]#(theta+1)^2*(theta+2)^2
  r_var <- vartheta/kappa2
  #r_var <- kappa2/vartheta
  return(r_var)
}

rate_v <- sapply(0.01:100, comp_var,mu = 1/9,v=0.1,d0 = 1e-10)
plot(0.01:350,rate_v,type = "l")

plot_raz <- function(lmax,mu,v,d0 = 1e-10){
     rate_v <- sapply(0.0001:lmax, comp_var,mu = mu,v=v,d0 = d0)
     plot(0.0001:lmax,rate_v,type = "l")
}

p_raz <- ggplot(data = data.frame(x = c(0.0001, 9)), mapping = aes(x)) +
         stat_function(fun = comp_var, args = list(mu=log(3),v=0.1,d0 = 1e-10)) 

plot_raz <- function(lmax,mu,v,d0 = 1e-10){
  x_raz <- seq(0.000001,lmax,1)
  rate_v <- sapply(x_raz, comp_var,mu = mu,v=v,d0 = d0)
  df_raz <- data.frame(x_raz,rate_v)
  selec_raz <- df_raz %>%  filter(rate_v>1)
  p_raz <- ggplot(df_raz, aes(x=x_raz, y=rate_v)) + 
    geom_line()  +
    labs( x = expression(theta),y = expression(V[hat(theta)[MV]]/V[hat(theta)[MF]])) +
    geom_hline(yintercept = 1, color = "red", linetype = 5,size=0.8)
  return(list(df_raz,selec_raz,p_raz))
}

### aumentndo o limite do eixo x

plot_raz(9,mu=log(3),v=0.1)
plot_raz(100,mu=log(3),v=0.1)
plot_raz(500,mu=log(3),v=0.1)
plot_raz(1000,mu=log(3),v=0.1)


### mudando v

plot_raz(500,mu=log(3),v=0.1)
plot_raz(500,mu=log(3),v=0.2)
plot_raz(500,mu=log(3),v=0.3)
plot_raz(500,mu=log(3),v=0.4)


#############


eqm <- function(mu,theta,v,d0){
  call_quant <- quantity_t(mu,theta,v,u=-v, d0=d0) ## chamar a função quantity_t
  kappa2 <- call_quant[1]*(call_quant[4])^2 # variância obtida pelo método delta para a inversa
  vartheta <- (theta+1)^2*(theta+2)^2
  eqm_mf <- kappa2 + (call_quant[3]-theta)^2
  eqm_mv <- vartheta
  #r_var <- kappa2/vartheta
  return(cbind(eqm_mf,eqm_mv,call_quant[3]))
}
eqm(mu = log(3),theta = 1/9,v=0.1,d0 = 1e-10)

