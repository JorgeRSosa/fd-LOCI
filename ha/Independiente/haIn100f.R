#####Para muestras de 100 curvas
source('Fijo/parametros fijos.R')
b <- 200
mc <- 1000


#####Para muestras de 100 curvas
prob100 <- list()
d100 <- list()

for(l in 1:length(m_f)){
  muestra <- list()
  for (i in 1:mc)
    
  {
    set.seed(i)
    muestra[[i]] <-  fdata(rbind(mvrnorm(99, m1(t), S),mvrnorm(1, m_f[[l]], S)),argvals = argvals)
  }
    d100[[l]] <- muestra
    save(d100,file = 'muestras100I_forma.Rdata')
    
    f1 <- lapply(muestra,function(x) fLOCI(x,alpha =  alpha,nn = n2/2,dist='L2'))
    f2 <- lapply(muestra,function(x) fLOCI(x,alpha =  alpha,nn = n2/2,dist='L2e'))
    a <- lapply(muestra,function(x) depth.R1(t(x$data)))
    f3 <- lapply(a,function(x) elastic_outliers(x))
    f4 <- lapply(muestra,function(x) outliers.depth.trim(x,nb=b))
    
    aL2n <- lapply(f1, function(x) ifelse(x$class[100]=="Outlier",1,0))
    aL2d <- lapply(f2,function(x) ifelse(x$class[100]=="Outlier",1,0))
    aDe <- lapply(f3,function(x) ifelse(x[[1]][100]==1,1,0))
    afda.usc <- lapply(f4,function(x) ifelse('100'%in%x$outliers,1,0))
    
    prob100[[l]] <- rbind(unlist(aL2n),unlist(aL2d),unlist(aDe),unlist(afda.usc))
    
}

save(prob100,file = 'prob100I_forma.RData')
