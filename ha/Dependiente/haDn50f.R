#####Para muestras de 50 curvas
source('Fijo/parametros fijos.R')
source('Fijo/func sim.R')
b <- 200
mc <- 1000


p50f <- list()
d50r <- list()
for(k in 1:length(rho)){
  prob50 <- list()
  d50 <- list()
  for(l in 1:length(m_f)){

    muestra <- list()
    for (i in 1:mc)
      
    {
      set.seed(i)
      muestra[[i]] <-  fdata(func.sim(n=n1,mdata = length(t),mu = m1(t),sigma = S,rho = rho[k],mu2 = m_f[[l]]),
                             argvals = argvals)
     }
      d50[[l]] <- muestra
      
      f1 <- lapply(muestra,function(x) fLOCI(x,alpha =  alpha,nn = n1/2,dist='L2'))
      f2 <- lapply(muestra,function(x) fLOCI(x,alpha =  alpha,nn = n1/2,dist='L2e'))
      a <- lapply(muestra,function(x) depth.R1(t(x$data)))
      f3 <- lapply(a,function(x) elastic_outliers(x))
      f4 <- lapply(muestra,function(x) outliers.depth.trim(x,nb=b))
      
      aL2n <- lapply(f1, function(x) ifelse(x$class[50]=="Outlier",1,0))
      aL2d <- lapply(f2,function(x) ifelse(x$class[50]=="Outlier",1,0))
      aDe <- lapply(f3,function(x) ifelse(x[[1]][50]==1,1,0))
      afda.usc <- lapply(f4,function(x) ifelse('50'%in%x$outliers,1,0))
      
      prob50[[l]] <- rbind(unlist(aL2n),unlist(aL2d),unlist(aDe),unlist(afda.usc))
  }
  d50r[[k]] <- d50
  save(d50r,file = 'muestra50D_forma_.Rdata')
  p50f[[k]] <- prob50
}

save(p50f,file = 'prob50D_forma.RData')
