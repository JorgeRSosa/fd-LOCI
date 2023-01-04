source('Fijo/parametros fijos.R')
mc <- 1000


## tabla para error tipo 1 muestra de tamaño 50 con curvas independientes
load('p50d_error1.RData')

M1 <- lapply(p50d, function (x) apply(x,1,function(y) sum(y)/mc))


## tabla para error tipo 1 muestra de tamaño 100 con curvas independientes
load('p100d_error1.RData')

M1 <- lapply(p50d, function (x) apply(x,1,function(y) sum(y)/mc))
