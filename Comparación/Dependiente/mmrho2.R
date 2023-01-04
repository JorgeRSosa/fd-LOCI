source('Fijo/parametros fijos.R')
source('Fijo/func sim.R')

b <- 500
mc <- 1000


# mmrho2 <- list()
# for(j in 1:length(m_m)){
#   for(i in 1:mc) {
#     set.seed(i)
#     mmrho2[[i]] <-  fdata(func.sim(n=no,mdata = length(t),mu = m1(t),sigma = S,rho = rho[2],mu2 = m_m[[j]],m=o),
#                            argvals = argvals)
#   }
# }
# 
# save(mmrho2,file = 'Rdata/mmrho2.Rdata')

load('Rdata/mmrho2.Rdata')


f1 <- lapply(mmrho2,function(x) fLOCI(x,alpha =  alpha,nn = n/2,dist='L2'))
f2 <- lapply(mmrho2,function(x) fLOCI(x,alpha =  alpha,nn = n/2,dist='L2e'))
#a <- lapply(mmrho2,function(x) depth.R1(t(x$data)))
#f3 <- lapply(a,function(x) elastic_outliers(x))
f4 <- lapply(mmrho2,function(x) outliers.depth.trim(x,nb=b))

result <- list(f1,f2,f4)
save(result,file = 'Rdata/result_mmrho2.Rdata')