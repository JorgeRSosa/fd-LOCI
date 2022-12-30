library(fda)
library(fda.usc)
library(MASS)
library(openxlsx)
source("LOCI.R")

m1 <- function(x) 30*x*(1-x)^(3/2) #funcion de media
C <- function(x, y) 0.3 * exp(-abs(x - y)/0.3) # funcion de covarianza
t <- seq(0,1,0.02)
k <- length(t) 
m <- m1(t)
S<-outer(t,t,C) #matriz de covarianza
# S <- matrix(nrow = k, ncol = k)                       calculo de la matriz de 
# for (i in 1:k) for (j in 1:k) S[i, j] = C(t[i], t[j]) covarianza


z <- mvrnorm(1, m, S)#distribucion normal multivariada que saca 1 muestra con media m y matriz de covarianza S
plot(z,type = 'l')




###########################################################################
##########parámetros algoritmo LOCI################
p1 <- seq(0.1,1,0.3) #parametro 1
#los radios tendran el 50%, 70% y 90% de las muestras

##################Suponiendo se cumple Ho ###############################
argvals <- 1:51

#######################################################################
######Para muestras de 50 curvas
# M25 <- c()
# M35 <- c()
# M45 <- c()
# for (i in 1:1000){
#   d50 <- fdata(mvrnorm(50, m, S),argvals = argvals)
#   aux1 <- c()
#   aux2 <- c()
#   aux3 <- c()
#   for(j in p1){
#     f1 <- fLOCI(d50,alpha =  j,nn = 25)
#     f2 <- fLOCI(d50,alpha =  j,nn = 35)
#     f3 <- fLOCI(d50,alpha =  j,nn = 45)
#     
#     aux1 <- cbind(aux1,sum(f1$class=="Outlier")/50)
#     aux2 <- cbind(aux2,sum(f2$class=="Outlier")/50)
#     aux3 <- cbind(aux3,sum(f3$class=="Outlier")/50)
#   }
#   M25 <- rbind(M25,aux1)
#   M35 <- rbind(M35,aux2)
#   M45 <- rbind(M45,aux3)
# }
# a25 <- apply(M25,2,function(x) sum(x)/1000)
# a35 <- apply(M35,2,function(x) sum(x)/1000)
# a45 <- apply(M45,2,function(x) sum(x)/1000)
# M2 <- rbind(a25,a35,a45)
# colnames(M2)<-paste0("p1 ",p1)
# rownames(M2) <- c(25,35,45)
# 
# save(M2,file = 'M2.RData')
load('M2.RData')

#########################################################################
######Para muestras de 100 curvas
M40 <- c()
M71 <- c()
M95 <- c()
for (i in 1:1000){
  d100 <- fdata(mvrnorm(100, m, S),argvals = argvals)
  aux1 <- c()
  aux2 <- c()
  aux3 <- c()
  for(j in p1){
    f1 <- fLOCI(d100,alpha =  j,nn = 50)
    f2 <- fLOCI(d100,alpha =  j,nn = 70)
    f3 <- fLOCI(d100,alpha =  j,nn = 90)

    aux1 <- cbind(aux1,sum(f1$class=="Outlier")/100)
    aux2 <- cbind(aux2,sum(f2$class=="Outlier")/100)
    aux3 <- cbind(aux3,sum(f3$class=="Outlier")/100)
  }
  M40 <- rbind(M40,aux1)
  M71 <- rbind(M71,aux2)
  M95 <- rbind(M95,aux3)
}
a40 <- apply(M40,2,function(x) sum(x)/1000)
a71 <- apply(M71,2,function(x) sum(x)/1000)
a95 <- apply(M95,2,function(x) sum(x)/1000)
M3 <- rbind(a40,a71,a95)
colnames(M3)<-paste0("p1 ",p1)
rownames(M3) <- c(50,70,90)

save(M3,file = 'M3.RData')
load('M3.RData')

######################################################################
######################Segunda simulación##############################
######################################################################

p12 <- seq(0.7,1,0.08) # parámetro 1 tomado otros valores

#######################################################################
######Para muestras de 50 curvas
# M25 <- c()
# M35 <- c()
# M45 <- c()
# for (i in 1:1000){
#   d50 <- fdata(mvrnorm(50, m, S),argvals = argvals)
#   aux1 <- c()
#   aux2 <- c()
#   aux3 <- c()
#   for(j in p12){
#     f1 <- fLOCI(d50,alpha =  j,nn = 25)
#     f2 <- fLOCI(d50,alpha =  j,nn = 35)
#     f3 <- fLOCI(d50,alpha =  j,nn = 45)
#     
#     aux1 <- cbind(aux1,sum(f1$class=="Outlier")/50)
#     aux2 <- cbind(aux2,sum(f2$class=="Outlier")/50)
#     aux3 <- cbind(aux3,sum(f3$class=="Outlier")/50)
#   }
#   M25 <- rbind(M25,aux1)
#   M35 <- rbind(M35,aux2)
#   M45 <- rbind(M45,aux3)
# }
# a25 <- apply(M25,2,function(x) sum(x)/1000)
# a35 <- apply(M35,2,function(x) sum(x)/1000)
# a45 <- apply(M45,2,function(x) sum(x)/1000)
# M5 <- rbind(a25,a35,a45)
# colnames(M5)<-paste0("p1 ",p12)
# rownames(M5) <- c(25,35,45)
# 
# save(M5,file = 'M5.RData')
load('M5.RData')

#########################################################################
######Para muestras de 100 curvas
M40 <- c()
M71 <- c()
M95 <- c()
for (i in 1:1000){
  d100 <- fdata(mvrnorm(100, m, S),argvals = argvals)
  aux1 <- c()
  aux2 <- c()
  aux3 <- c()
  for(j in p12){
    f1 <- fLOCI(d100,alpha =  j,nn = 50)
    f2 <- fLOCI(d100,alpha =  j,nn = 70)
    f3 <- fLOCI(d100,alpha =  j,nn = 90)

    aux1 <- cbind(aux1,sum(f1$class=="Outlier")/100)
    aux2 <- cbind(aux2,sum(f2$class=="Outlier")/100)
    aux3 <- cbind(aux3,sum(f3$class=="Outlier")/100)
  }
  M40 <- rbind(M40,aux1)
  M71 <- rbind(M71,aux2)
  M95 <- rbind(M95,aux3)
}
a40 <- apply(M40,2,function(x) sum(x)/1000)
a71 <- apply(M71,2,function(x) sum(x)/1000)
a95 <- apply(M95,2,function(x) sum(x)/1000)
M6 <- rbind(a40,a71,a95)
colnames(M6)<-paste0("p1 ",p12)
rownames(M6) <- c(50,70,90)

save(M6,file = 'M6.RData')
load('M6.RData')


######################################################################
#################Suponiendo no cumple Ho  ############################
######################################################################
m2 <- function(x) (30*(x^(3/2))*(1-x)) # funcion de media para atípico

gama <- seq(0.2,1,0.2)
delta <- seq(0.4,2,0.4)

########## variar los parametros de fLOCI

radio50 <- c(25,35,45)
radio100 <- c(40,71,95)

p12 <- seq(0.7,1,0.08)
################################################################################
#Para simular por forma

m_f <- list()
for(i in 1:length(gama)){
  m_f[[i]] <- (1-gama[i])*m1(t)+gama[i]*m2(t)
  }



######Para muestras de 50 curvas
# prob50 <- list()
# 
# for(l in 1:length(m_f)){
# M25 <- c()
# M35 <- c()
# M45 <- c()
# for (i in 1:1000){
#   d50 <- fdata(rbind(mvrnorm(49, m, S),mvrnorm(1, m_f[[l]], S)),argvals = argvals)
#   aux1 <- c()
#   aux2 <- c()
#   aux3 <- c()
#   for(j in p12){
#     f1 <- fLOCI(d50,alpha =  j,nn = 25)
#     f2 <- fLOCI(d50,alpha =  j,nn = 35)
#     f3 <- fLOCI(d50,alpha =  j,nn = 45)
#     
#     aux1 <- cbind(aux1,ifelse(f1$class[50]=="Outlier",1,0))
#     aux2 <- cbind(aux2,ifelse(f2$class[50]=="Outlier",1,0))
#     aux3 <- cbind(aux3,ifelse(f3$class[50]=="Outlier",1,0))
#   }
#   M25 <- rbind(M25,aux1)
#   M35 <- rbind(M35,aux2)
#   M45 <- rbind(M45,aux3)
# }
# a25 <- apply(M25,2,function(x) sum(x)/1000)
# a35 <- apply(M35,2,function(x) sum(x)/1000)
# a45 <- apply(M45,2,function(x) sum(x)/1000)
# M5 <- rbind(a25,a35,a45)
# colnames(M5)<-paste0("p1 ",p12)
# rownames(M5) <- c(25,35,45)
# prob50[[l]] <- M5
# }
# 
# prob50[[1]]
# save(prob50,file = 'prob50.RData')
load('prob50.RData')

######Para muestras de 100 curvas
# prob100 <- list()
# for(l in 1:length(m_f)){
# M40 <- c()
# M71 <- c()
# M95 <- c()
# for (i in 1:1000){
#   d100 <- fdata(rbind(mvrnorm(99, m, S),mvrnorm(1, m_f[[l]], S)),argvals = argvals)
#   aux1 <- c()
#   aux2 <- c()
#   aux3 <- c()
#   for(j in p12){
#     f1 <- fLOCI(d100,alpha =  j,nn = 50)
#     f2 <- fLOCI(d100,alpha =  j,nn = 70)
#     f3 <- fLOCI(d100,alpha =  j,nn = 90)
#     
#     aux1 <- cbind(aux1,ifelse(f1$class[100]=="Outlier",1,0))
#     aux2 <- cbind(aux2,ifelse(f2$class[100]=="Outlier",1,0))
#     aux3 <- cbind(aux3,ifelse(f3$class[100]=="Outlier",1,0))
#   }
#   M40 <- rbind(M40,aux1)
#   M71 <- rbind(M71,aux2)
#   M95 <- rbind(M95,aux3)
# }
# a40 <- apply(M40,2,function(x) sum(x)/1000)
# a71 <- apply(M71,2,function(x) sum(x)/1000)
# a95 <- apply(M95,2,function(x) sum(x)/1000)
# M6 <- rbind(a40,a71,a95)
# colnames(M6)<-paste0("p1 ",p12)
# rownames(M6) <- c(50,70,90)
# prob100[[l]] <- M6
# }
# 
# prob100[[1]]
# save(prob100,file = 'prob100.RData')
load('prob100.RData')

#################################################################################
#Para simular por magnitud

m_m <- list()
for(i in 1:length(delta)) m_m[[i]] <- m1(t) + delta[i]



######Para muestras de 50 curvas
# p50m <- list()
# for(l in 1:length(m_m)){
#   M25 <- c()
#   M35 <- c()
#   M45 <- c()
#   for (i in 1:1000){
#     d50 <- fdata(rbind(mvrnorm(49, m, S),mvrnorm(1, m_m[[l]], S)),argvals = argvals)
#     aux1 <- c()
#     aux2 <- c()
#     aux3 <- c()
#     for(j in p12){
#       f1 <- fLOCI(d50,alpha =  j,nn = 25)
#       f2 <- fLOCI(d50,alpha =  j,nn = 35)
#       f3 <- fLOCI(d50,alpha =  j,nn = 45)
#       
#       aux1 <- cbind(aux1,ifelse(f1$class[50]=="Outlier",1,0))
#       aux2 <- cbind(aux2,ifelse(f2$class[50]=="Outlier",1,0))
#       aux3 <- cbind(aux3,ifelse(f3$class[50]=="Outlier",1,0))
#     }
#     M25 <- rbind(M25,aux1)
#     M35 <- rbind(M35,aux2)
#     M45 <- rbind(M45,aux3)
#   }
#   a25 <- apply(M25,2,function(x) sum(x)/1000)
#   a35 <- apply(M35,2,function(x) sum(x)/1000)
#   a45 <- apply(M45,2,function(x) sum(x)/1000)
#   M5 <- rbind(a25,a35,a45)
#   colnames(M5)<-paste0("p1 ",p12)
#   rownames(M5) <- c(25,35,45)
#   p50m[[l]] <- M5
# }
# 
# p50m[[1]]
# save(p50m,file = 'p50m.RData')
load('p50m.RData')


######Para muestras de 100 curvas
# p100m <- list()
# 
# for(l in 1:length(m_m)){
#   M40 <- c()
#   M71 <- c()
#   M95 <- c()
#   for (i in 1:1000){
#     d100 <- fdata(rbind(mvrnorm(99, m, S),mvrnorm(1, m_m[[l]], S)),argvals = argvals)
#     aux1 <- c()
#     aux2 <- c()
#     aux3 <- c()
#     for(j in p12){
#       f1 <- fLOCI(d100,alpha =  j,nn = 50)
#       f2 <- fLOCI(d100,alpha =  j,nn = 70)
#       f3 <- fLOCI(d100,alpha =  j,nn = 90)
#       
#       aux1 <- cbind(aux1,ifelse(f1$class[100]=="Outlier",1,0))
#       aux2 <- cbind(aux2,ifelse(f2$class[100]=="Outlier",1,0))
#       aux3 <- cbind(aux3,ifelse(f3$class[100]=="Outlier",1,0))
#     }
#     M40 <- rbind(M40,aux1)
#     M71 <- rbind(M71,aux2)
#     M95 <- rbind(M95,aux3)
#   }
#   a40 <- apply(M40,2,function(x) sum(x)/1000)
#   a71 <- apply(M71,2,function(x) sum(x)/1000)
#   a95 <- apply(M95,2,function(x) sum(x)/1000)
#   M6 <- rbind(a40,a71,a95)
#   colnames(M6)<-paste0("p1 ",p12)
#   rownames(M6) <- c(50,70,90)
#   p100m[[l]] <- M6
# }
# 
# p100m[[1]]
# save(p100m,file = 'p100m.RData')
load('p100m.RData')

###################################################################
###################################################################
#######################caso dependiente############################

m1 <- function(x) 30*x*(1-x)^(3/2) #funcion de media
m2 <- function(x) (30*(x^(3/2))*(1-x)) # funcion de media para atípico
C <- function(x, y) 0.3 * exp(-abs(x - y)/0.3) # funcion de covarianza
t <- seq(0,1,0.02)
k <- length(t) 
m <- m1(t)
S<-outer(t,t,C)
argvals <- 1:51
p1 <- seq(0.1,1,0.3)

func.sim <- function(n=2,mdata,mu,sigma,rho=0,mu2=NULL){
  #n numero de curvas
  #mdata cuantos puntos tiene cada curva
  #mu media
  #mu2 es la media de la hipótesis alternativa y simular datos atípicos 
  #sigma matriz de varianzas y covarianzas
  #rho factor de dependencia
  if (rho != 0) sigma <- sigma * sqrt((1+rho)/(1-rho))
  C=svd(t(sigma))
  L.corr.teor=(C$u%*%diag(sqrt(C$d)))
  
  # Generacion de datos simulados: Y = m(X) + s(X) * E
  data.err <- L.corr.teor%*%matrix(rnorm(mdata * n), nrow = mdata)
  #data.err <- data.err * sqrt((1-rho)/(1+rho))
  err <- matrix(nrow = mdata, ncol = n)
  res <- matrix(nrow = mdata, ncol = n)
  if(is.null(mu2)){
    if (rho != 0) {
      err[, 1] <- data.err[, 1] * sqrt((1-rho)/(1+rho))
      for(i in 2:n)
        err[, i] <- rho*err[, i-1] + (1 - rho)*data.err[, i]
    }
    #Datos simulados: Y = m(x) + S(x)*E
    res <- drop(mu) + err
  }
  else{
    if (rho != 0) {
      err[, 1] <- data.err[, 1] * sqrt((1-rho)/(1+rho))
      for(i in 2:n)
        err[, i] <- rho*err[, i-1] + (1 - rho)*data.err[, i]
    }
    #Datos simulados: Y = m(x) + S(x)*E
    res[,1:n-1] <- drop(mu) + err[,1:n-1]
    res[,n] <- drop(mu2) + err[,n]
  }
  return(t(res))
}

z <- func.sim(n=50,mdata = length(t),mu = m,sigma = S,rho = -0.7,mu2 = m_f[[2]])
plot(x=1:51,z[1,],type = 'l')
for(i in 1:49) lines(x=1:51,z[i+1,])

rho <- c(-0.7,-0.3,0.3,0.7)

#######################################################################
######Para muestras de 50 curvas
# p50d <- list()
# for(k in 1:length(rho)){
# M25 <- c()
# M35 <- c()
# M45 <- c()
# for (i in 1:1000){
#   d50 <- fdata(func.sim(n=50,mdata = length(t),mu = m,sigma = S,rho = rho[k]),argvals = argvals)
#   aux1 <- c()
#   aux2 <- c()
#   aux3 <- c()
#   for(j in p1){
#     f1 <- fLOCI(d50,alpha =  j,nn = 25)
#     f2 <- fLOCI(d50,alpha =  j,nn = 35)
#     f3 <- fLOCI(d50,alpha =  j,nn = 45)
# 
#     aux1 <- cbind(aux1,sum(f1$class=="Outlier")/50)
#     aux2 <- cbind(aux2,sum(f2$class=="Outlier")/50)
#     aux3 <- cbind(aux3,sum(f3$class=="Outlier")/50)
#   }
#   M25 <- rbind(M25,aux1)
#   M35 <- rbind(M35,aux2)
#   M45 <- rbind(M45,aux3)
# }
# a25 <- apply(M25,2,function(x) sum(x)/1000)
# a35 <- apply(M35,2,function(x) sum(x)/1000)
# a45 <- apply(M45,2,function(x) sum(x)/1000)
# M2 <- rbind(a25,a35,a45)
# colnames(M2)<-paste0("p1 ",p1)
# rownames(M2) <- c(25,35,45)
# p50d[[k]] <- M2
# }
# save(p50d,file = 'p50d.RData')
load('p50d.RData')

#########################################################################
######Para muestras de 100 curvas
# p100d <- list()
# for(k in 1:length(rho)){
# M40 <- c()
# M71 <- c()
# M95 <- c()
# for (i in 1:1000){
#   d100 <- fdata(func.sim(n=100,mdata = length(t),mu = m,sigma = S,rho = rho[k]),argvals = argvals)
#   aux1 <- c()
#   aux2 <- c()
#   aux3 <- c()
#   for(j in p1){
#     f1 <- fLOCI(d100,alpha =  j,nn = 50)
#     f2 <- fLOCI(d100,alpha =  j,nn = 70)
#     f3 <- fLOCI(d100,alpha =  j,nn = 90)
#     
#     aux1 <- cbind(aux1,sum(f1$class=="Outlier")/100)
#     aux2 <- cbind(aux2,sum(f2$class=="Outlier")/100)
#     aux3 <- cbind(aux3,sum(f3$class=="Outlier")/100)
#   }
#   M40 <- rbind(M40,aux1)
#   M71 <- rbind(M71,aux2)
#   M95 <- rbind(M95,aux3)
# }
# a40 <- apply(M40,2,function(x) sum(x)/1000)
# a71 <- apply(M71,2,function(x) sum(x)/1000)
# a95 <- apply(M95,2,function(x) sum(x)/1000)
# M3 <- rbind(a40,a71,a95)
# colnames(M3)<-paste0("p1 ",p1)
# rownames(M3) <- c(50,70,90)
# p100d[[k]] <- M3
# }
# save(p100d,file = 'p100d.RData')
load('p100d.RData')


##################################################################
################### segunda simulacion#############################

p12 <- seq(0.7,1,0.08) # parámetro 1 tomado otros valores



#######################################################################
######Para muestras de 50 curvas
p50d_ <- list()
for(k in 1:length(rho)){
  M25 <- c()
  M35 <- c()
  M45 <- c()
  for (i in 1:1000){
    d50 <- fdata(func.sim(n=50,mdata = length(t),mu = m,sigma = S,rho = rho[k]),argvals = argvals)
    aux1 <- c()
    aux2 <- c()
    aux3 <- c()
    for(j in p12){
      f1 <- fLOCI(d50,alpha =  j,nn = 25)
      f2 <- fLOCI(d50,alpha =  j,nn = 35)
      f3 <- fLOCI(d50,alpha =  j,nn = 45)

      aux1 <- cbind(aux1,sum(f1$class=="Outlier")/50)
      aux2 <- cbind(aux2,sum(f2$class=="Outlier")/50)
      aux3 <- cbind(aux3,sum(f3$class=="Outlier")/50)
    }
    M25 <- rbind(M25,aux1)
    M35 <- rbind(M35,aux2)
    M45 <- rbind(M45,aux3)
  }
  a25 <- apply(M25,2,function(x) sum(x)/1000)
  a35 <- apply(M35,2,function(x) sum(x)/1000)
  a45 <- apply(M45,2,function(x) sum(x)/1000)
  M2 <- rbind(a25,a35,a45)
  colnames(M2)<-paste0("p1 ",p12)
  rownames(M2) <- c(25,35,45)
  p50d_[[k]] <- M2
}
save(p50d_,file = 'p50d_.RData')
load('p50d_.RData')

#########################################################################
######Para muestras de 100 curvas
p100d_ <- list()
for(k in 1:length(rho)){
  M40 <- c()
  M71 <- c()
  M95 <- c()
  for (i in 1:1000){
    d100 <- fdata(func.sim(n=100,mdata = length(t),mu = m,sigma = S,rho = rho[k]),argvals = argvals)
    aux1 <- c()
    aux2 <- c()
    aux3 <- c()
    for(j in p12){
      f1 <- fLOCI(d100,alpha =  j,nn = 50)
      f2 <- fLOCI(d100,alpha =  j,nn = 70)
      f3 <- fLOCI(d100,alpha =  j,nn = 90)

      aux1 <- cbind(aux1,sum(f1$class=="Outlier")/100)
      aux2 <- cbind(aux2,sum(f2$class=="Outlier")/100)
      aux3 <- cbind(aux3,sum(f3$class=="Outlier")/100)
    }
    M40 <- rbind(M40,aux1)
    M71 <- rbind(M71,aux2)
    M95 <- rbind(M95,aux3)
  }
  a40 <- apply(M40,2,function(x) sum(x)/1000)
  a71 <- apply(M71,2,function(x) sum(x)/1000)
  a95 <- apply(M95,2,function(x) sum(x)/1000)
  M3 <- rbind(a40,a71,a95)
  colnames(M3)<-paste0("p1 ",p12)
  rownames(M3) <- c(50,70,90)
  p100d_[[k]] <- M3
}
save(p100d_,file = 'p100d_.RData')
load('p100d_.RData')


################################################################################
######################### Suponiendo H_0 no se cumple###########################
gama <- seq(0.2,1,0.2)
delta <- seq(0.4,2,0.4)

p12 <- seq(0.7,1,0.08)
################################################################################
#Para simular por forma y dependencia

m_f <- list()
for(i in 1:length(gama)){
  m_f[[i]] <- (1-gama[i])*m1(t)+gama[i]*m2(t)
}


######Para muestras de 50 curvas
p50f <- list()
for(k in 1:length(rho)){
  prob50 <- list()
  for(l in 1:length(m_f)){
    M25 <- c()
    M35 <- c()
    M45 <- c()
    for (i in 1:1000){
      d50 <- fdata(func.sim(n=50,mdata = length(t),mu = m,sigma = S,rho = rho[k],mu2 = m_f[[l]]),
                   argvals = argvals)
      aux1 <- c()
      aux2 <- c()
      aux3 <- c()
      for(j in p12){
        f1 <- fLOCI(d50,alpha =  j,nn = 25)
        f2 <- fLOCI(d50,alpha =  j,nn = 35)
        f3 <- fLOCI(d50,alpha =  j,nn = 45)

        aux1 <- cbind(aux1,ifelse(f1$class[50]=="Outlier",1,0))
        aux2 <- cbind(aux2,ifelse(f2$class[50]=="Outlier",1,0))
        aux3 <- cbind(aux3,ifelse(f3$class[50]=="Outlier",1,0))
      }
      M25 <- rbind(M25,aux1)
      M35 <- rbind(M35,aux2)
      M45 <- rbind(M45,aux3)
    }
    a25 <- apply(M25,2,function(x) sum(x)/1000)
    a35 <- apply(M35,2,function(x) sum(x)/1000)
    a45 <- apply(M45,2,function(x) sum(x)/1000)
    M5 <- rbind(a25,a35,a45)
    colnames(M5)<-paste0("p1 ",p12)
    rownames(M5) <- c(25,35,45)
    prob50[[l]] <- M5
  }
p50f[[k]] <- prob50
}

save(p50f,file = 'p50f.RData')
load('p50f.RData')

######Para muestras de 100 curvas
p100f <- list()
for(k in 1:length(rho)){
  prob100 <- list()
  for(l in 1:length(m_f)){
    M40 <- c()
    M71 <- c()
    M95 <- c()
    for (i in 1:1000){
      d100 <- fdata(func.sim(n=100,mdata = length(t),mu = m,sigma = S,rho = rho[k],mu2 = m_f[[l]]),
                    argvals = argvals)
      aux1 <- c()
      aux2 <- c()
      aux3 <- c()
      for(j in p12){
        f1 <- fLOCI(d100,alpha =  j,nn = 50)
        f2 <- fLOCI(d100,alpha =  j,nn = 70)
        f3 <- fLOCI(d100,alpha =  j,nn = 90)

        aux1 <- cbind(aux1,ifelse(f1$class[100]=="Outlier",1,0))
        aux2 <- cbind(aux2,ifelse(f2$class[100]=="Outlier",1,0))
        aux3 <- cbind(aux3,ifelse(f3$class[100]=="Outlier",1,0))
      }
      M40 <- rbind(M40,aux1)
      M71 <- rbind(M71,aux2)
      M95 <- rbind(M95,aux3)
    }
    a40 <- apply(M40,2,function(x) sum(x)/1000)
    a71 <- apply(M71,2,function(x) sum(x)/1000)
    a95 <- apply(M95,2,function(x) sum(x)/1000)
    M6 <- rbind(a40,a71,a95)
    colnames(M6)<-paste0("p1 ",p12)
    rownames(M6) <- c(50,70,90)
    prob100[[l]] <- M6
  }
p100f[[k]] <- prob100
}

save(p100f,file = 'p100f.RData')
load('p100f.RData')

#################################################################################
#Para simular por magnitud y dependencia

m_m <- list()
for(i in 1:length(delta)) m_m[[i]] <- m1(t) + delta[i]



######Para muestras de 50 curvas
p50f_m <- list()
for(k in 1:length(rho)){
  p50m <- list()
  for(l in 1:length(m_m)){
    M25 <- c()
    M35 <- c()
    M45 <- c()
    for (i in 1:1000){
      d50 <- fdata(func.sim(n=50,mdata = length(t),mu = m,sigma = S,rho = rho[k],mu2 = m_m[[l]]),
                   argvals = argvals)
      aux1 <- c()
      aux2 <- c()
      aux3 <- c()
      for(j in p12){
        f1 <- fLOCI(d50,alpha =  j,nn = 25)
        f2 <- fLOCI(d50,alpha =  j,nn = 35)
        f3 <- fLOCI(d50,alpha =  j,nn = 45)

        aux1 <- cbind(aux1,ifelse(f1$class[50]=="Outlier",1,0))
        aux2 <- cbind(aux2,ifelse(f2$class[50]=="Outlier",1,0))
        aux3 <- cbind(aux3,ifelse(f3$class[50]=="Outlier",1,0))
      }
      M25 <- rbind(M25,aux1)
      M35 <- rbind(M35,aux2)
      M45 <- rbind(M45,aux3)
    }
    a25 <- apply(M25,2,function(x) sum(x)/1000)
    a35 <- apply(M35,2,function(x) sum(x)/1000)
    a45 <- apply(M45,2,function(x) sum(x)/1000)
    M5 <- rbind(a25,a35,a45)
    colnames(M5)<-paste0("p1 ",p12)
    rownames(M5) <- c(25,35,45)
    p50m[[l]] <- M5
  }
p50f_m[[k]] <- p50m
}

save(p50f_m,file = 'p50f_m.RData')
load('p50f_m.RData')


######Para muestras de 100 curvas
p100f_m <- list()
for(k in 1:length(rho)){
  p100m <- list()
  for(l in 1:length(m_m)){
    M40 <- c()
    M71 <- c()
    M95 <- c()
    for (i in 1:1000){
      d100 <- fdata(func.sim(n=100,mdata = length(t),mu = m,sigma = S,rho = rho[k],mu2 = m_m[[l]]),
                    argvals = argvals)
      aux1 <- c()
      aux2 <- c()
      aux3 <- c()
      for(j in p12){
        f1 <- fLOCI(d100,alpha =  j,nn = 50)
        f2 <- fLOCI(d100,alpha =  j,nn = 70)
        f3 <- fLOCI(d100,alpha =  j,nn = 90)

        aux1 <- cbind(aux1,ifelse(f1$class[100]=="Outlier",1,0))
        aux2 <- cbind(aux2,ifelse(f2$class[100]=="Outlier",1,0))
        aux3 <- cbind(aux3,ifelse(f3$class[100]=="Outlier",1,0))
      }
      M40 <- rbind(M40,aux1)
      M71 <- rbind(M71,aux2)
      M95 <- rbind(M95,aux3)
    }
    a40 <- apply(M40,2,function(x) sum(x)/1000)
    a71 <- apply(M71,2,function(x) sum(x)/1000)
    a95 <- apply(M95,2,function(x) sum(x)/1000)
    M6 <- rbind(a40,a71,a95)
    colnames(M6)<-paste0("p1 ",p12)
    rownames(M6) <- c(50,70,90)
    p100m[[l]] <- M6
  }
p100f_m[[k]] <- p100m
}

save(p100f_m,file = 'p100f_m.RData')
load('p100f_m.RData')




