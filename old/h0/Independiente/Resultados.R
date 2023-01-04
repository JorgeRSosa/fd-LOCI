source('Fijo/parametros fijos.R')
mc <- 1000


## tabla para error tipo 1 muestra de tamaño 50 con curvas independientes
load("C:/Users/Jorge/Downloads/ATT30619 (1).RData")

rownames(result)<-c("LOCI L2 ","LOCI L2 opt","Depht elast","O. fda.usc")
M50 <- apply(result,1,function(x) sum(x)/mc)


## tabla para error tipo 1 muestra de tamaño 100 con curvas independientes
load("C:/Users/Jorge/Documents/Practicas/Prácticas Logika/Datos_Funcionales/fd-LOCI/fd-LOCI/Rdata/In100.RData")

rownames(result)<-c("LOCI L2 ","LOCI L2 opt","Depht elast","O. fda.usc")
M100 <- apply(result,1,function(x) sum(x)/mc)


# tabla para las dos muestras
M <- rbind(M50,M100)
rownames(M) <- c('n = 50','n = 100')
