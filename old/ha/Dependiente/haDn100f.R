#####Para muestras de 100 curvas
setwd("~/LOCI")
source('Fijo/parametros fijos.R')
source('Fijo/func sim.R')
b <- 200
mc <- 1000


p100f <- list()
d100r <- list()
for(k in 1:length(rho)){
  d100 <- list()
  prob100 <- list()
  for(l in 1:length(m_f)){

    muestra <- list()
    for (i in 1:mc)
      
    {
      set.seed(i)
      muestra[[i]] <-  fdata(func.sim(n=n2,mdata = length(t),mu = m1(t),sigma = S,rho = rho[k],mu2 = m_f[[l]]),
                             argvals = argvals)
    }
    d100[[l]] <- muestra
      
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
  d100r[[k]] <- d100
  save(d100r,file = 'Rdata/muestra100D_forma_.Rdata')
  p100f[[k]] <- prob100
}

save(p100f,file = 'Rdata/prob100D_forma.RData')
