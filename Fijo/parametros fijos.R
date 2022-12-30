library(fda)
library(fda.usc)
library(MASS)
library(openxlsx)
library(elasticdepth)
library(future)
library(future.apply)
library(fdasrvf)

#if (require("fdasrvf")) install.packages("fdasrvf")
#if (require("devtools")) install.packages("devtools")
#if (require("elasticdepth")) install_github("trevor-harris/elasticdepth")
#install.packages("devtools")
#library(devtools)
#install_github("trevor-harris/elasticdepth")

source("Fijo/nuevo LOCI.R")

m1 <- function(x) 30*x*(1-x)^(3/2) #funcion de media
C <- function(x, y) 0.3 * exp(-abs(x - y)/0.3) # funcion de covarianza
t <- seq(0,1,0.02)
S<-outer(t,t,C) #matriz de covarianza

argvals <- 1:51
alpha <- 0.78
n1 <- 50
n2 <- 100

m2 <- function(x) (30*(x^(3/2))*(1-x)) # funcion de media para atípico

gama <- seq(0.2,1,0.2)
delta <- seq(0.4,2,0.4)
#Para simular por forma

m_f <- list()
for(i in 1:length(gama)) m_f[[i]] <- (1-gama[i])*m1(t)+gama[i]*m2(t)

#Para simular por magnitud

m_m <- list()
for(i in 1:length(delta)) m_m[[i]] <- m1(t) + delta[i]