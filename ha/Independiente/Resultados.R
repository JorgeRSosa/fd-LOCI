source('Fijo/parametros fijos.R')
mc <- 1000

## tabla para potencia muestra de tamaño 50 con curvas independientes variacion forma
load('prob50I_forma.RData')

M1 <- lapply(prob50, function (x) apply(x,1,function(y) sum(y)/mc))

M1 <- cbind(M1[[1]],M1[[2]],M1[[3]],M1[[4]],M1[[5]])
rownames(M1)<-c("LOCI L2 ","LOCI L2 opt","Depht elast","O. fda.usc")
colnames(M1)<-paste0("m_f ",m_f)
## tabla para potencia muestra de tamaño 100 con curvas independientes variacion forma
load('prob100I_forma.RData')

M1 <- lapply(prob100, function (x) apply(x,1,function(y) sum(y)/mc))

M1 <- cbind(M1[[1]],M1[[2]],M1[[3]],M1[[4]],M1[[5]])
rownames(M1)<-c("LOCI L2 ","LOCI L2 opt","Depht elast","O. fda.usc")
colnames(M1)<-paste0("m_f ",m_f)


## tabla para potencia muestra de tamaño 50 con curvas independientes variacion magnitud
load('prob50I_magnitud.RData')

M1 <- lapply(prob50, function (x) apply(x,1,function(y) sum(y)/mc))

M1 <- cbind(M1[[1]],M1[[2]],M1[[3]],M1[[4]],M1[[5]])
rownames(M1)<-c("LOCI L2 ","LOCI L2 opt","Depht elast","O. fda.usc")
colnames(M1)<-paste0("m_f ",m_f)
## tabla para potencia muestra de tamaño 100 con curvas independientes variacion magnitud
load('prob100I_magnitud.RData')

M1 <- lapply(prob100, function (x) apply(x,1,function(y) sum(y)/mc))

M1 <- cbind(M1[[1]],M1[[2]],M1[[3]],M1[[4]],M1[[5]])
rownames(M1)<-c("LOCI L2 ","LOCI L2 opt","Depht elast","O. fda.usc")
colnames(M1)<-paste0("m_f ",m_f)