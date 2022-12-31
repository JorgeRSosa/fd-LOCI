#####Para muestras de 100 curvas
setwd("~/LOCI")
source('Fijo/parametros fijos.R')
b <- 200
mc <- 1000


#####Para muestras de 100 curvas
prob100 <- list()
d100 <- list()
load('muestra100I.Rdata')


for(l in 1:length(m_f)){
  
  aux <- apply(mvrnorm(mc, m_f[[l]], S),MARGIN = 1,function(x) fdata(x,argvals = argvals))
  for(i in 1:mc) muestra100[[i]] <- c(muestra100[[i]],aux[[i]]) 
  
    d100[[l]] <- muestra100
    save(d100,file = 'Rdata/muestras100I_forma.Rdata')
    
    f1 <- lapply(muestra100,function(x) fLOCI(x,alpha =  alpha,nn = n2/2,dist='L2',nindex=101))
    f2 <- lapply(muestra100,function(x) fLOCI(x,alpha =  alpha,nn = n2/2,dist='L2e',nindex=101))
    a <- lapply(muestra100,function(x) depth.R1(t(x$data)))
    f3 <- lapply(a,function(x) elastic_outliers(x))
    f4 <- lapply(muestra100,function(x) outliers.depth.trim(x,nb=b))
    
    aL2n <- lapply(f1, function(x) ifelse(x$class[101]=="Outlier",1,0))
    aL2d <- lapply(f2,function(x) ifelse(x$class[101]=="Outlier",1,0))
    aDe <- lapply(f3,function(x) ifelse(x[[1]][101]==1,1,0))
    afda.usc <- lapply(f4,function(x) ifelse('101'%in%x$outliers,1,0))
    
    prob100[[l]] <- rbind(unlist(aL2n),unlist(aL2d),unlist(aDe),unlist(afda.usc))
    
}

save(prob100,file = 'Rdata/prob100I_forma.RData')
