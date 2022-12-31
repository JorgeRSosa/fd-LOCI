#####Para muestras de 50 curvas
setwd("~/LOCI")
source('Fijo/parametros fijos.R')
b <- 200
mc <- 1000

prob50 <- list()
d50 <- list()
load('muestra50I.Rdata')

for(l in 1:length(m_m)){
  aux <- apply(mvrnorm(mc, m_m[[l]], S),MARGIN = 1,function(x) fdata(x,argvals = argvals))
  for(i in 1:mc) muestra50[[i]] <- c(muestra50[[i]],aux[[i]]) 
  
    d50[[l]] <- muestra50
    save(d50,file = 'Rdata/muestra50I_magnitud.Rdata')
    
    f1 <- lapply(muestra50,function(x) fLOCI(x,alpha =  alpha,nn = n1/2,dist='L2',nindex=51))
    f2 <- lapply(muestra50,function(x) fLOCI(x,alpha =  alpha,nn = n1/2,dist='L2e',nindex=51))
    a <- lapply(muestra50,function(x) depth.R1(t(x$data)))
    f3 <- lapply(a,function(x) elastic_outliers(x))
    f4 <- lapply(muestra50,function(x) outliers.depth.trim(x,nb=b))
    
    aL2n <- lapply(f1, function(x) ifelse(x$class[51]=="Outlier",1,0))
    aL2d <- lapply(f2,function(x) ifelse(x$class[51]=="Outlier",1,0))
    aDe <- lapply(f3,function(x) ifelse(x[[1]][51]==1,1,0))
    afda.usc <- lapply(f4,function(x) ifelse('51'%in%x$outliers,1,0))
    
    prob50[[l]] <- rbind(unlist(aL2n),unlist(aL2d),unlist(aDe),unlist(afda.usc))
    
}

save(prob50,file = 'Rdata/prob50I_magnitud.RData')

