source('Fijo/parametros fijos.R')
mc <- 1000


## tabla para error tipo 1 muestra de tamaño 50 con curvas independientes
load("In50.RData")
M1 <- rbind(aL2n,aL2d,aDe,afda.usc)
rownames(M1)<-c("LOCI L2 ","LOCI L2 opt","Depht elast","O. fda.usc")
M1 <- apply(M1,1,function(x) sum(x)/mc)


## tabla para error tipo 1 muestra de tamaño 100 con curvas independientes
load("In100.RData")
M1 <- rbind(aL2n,aL2d,aDe,afda.usc)
rownames(M1)<-c("LOCI L2 ","LOCI L2 opt","Depht elast","O. fda.usc")
M1 <- apply(M1,1,function(x) sum(x)/mc)
