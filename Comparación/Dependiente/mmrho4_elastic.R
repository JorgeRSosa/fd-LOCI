source('Fijo/parametros fijos.R')
source('Fijo/func sim.R')

b <- 500
mc <- 1000


# mmrho4 <- list()
# for(j in 1:length(m_m)){
#   for(i in 1:mc) {
#     set.seed(i)
#     mmrho4[[i]] <-  fdata(func.sim(n=no,mdata = length(t),mu = m1(t),sigma = S,rho = rho[4],mu2 = m_m[[j]],m=o),
#                            argvals = argvals)
#   }
# }
# 
# save(mmrho4,file = 'Rdata/mmrho4.Rdata')

load('Rdata/mmrho4.Rdata')


#f1 <- lapply(mmrho4,function(x) fLOCI(x,alpha =  alpha,nn = n/2,dist='L2'))
#f2 <- lapply(mmrho4,function(x) fLOCI(x,alpha =  alpha,nn = n/2,dist='L2e'))
a <- lapply(mmrho4,function(x) depth.R1(t(x$data)))
f3 <- lapply(a,function(x) elastic_outliers(x))
#f4 <- lapply(mmrho4,function(x) outliers.depth.trim(x,nb=b))


save(f3,file = 'Rdata/f3_mmrho4.Rdata')