source('Fijo/parametros fijos.R')
source('Fijo/func sim.R')

b <- 500
mc <- 1000


# mfrho2 <- list()
# for(j in 1:length(m_f)){
#   for(i in 1:mc) {
#     set.seed(i)
#     mfrho2[[i]] <-  fdata(func.sim(n=no,mdata = length(t),mu = m1(t),sigma = S,rho = rho[2],mu2 = m_f[[j]],m=o),
#                            argvals = argvals)
#   }
# }
# 
# save(mfrho2,file = 'Rdata/mfrho2.Rdata')

load('Rdata/mfrho2.Rdata')


#f1 <- lapply(mfrho2,function(x) fLOCI(x,alpha =  alpha,nn = n/2,dist='L2'))
#f2 <- lapply(mfrho2,function(x) fLOCI(x,alpha =  alpha,nn = n/2,dist='L2e'))
a <- lapply(mfrho2,function(x) depth.R1(t(x$data)))
f3 <- lapply(a,function(x) elastic_outliers(x))
#f4 <- lapply(mfrho2,function(x) outliers.depth.trim(x,nb=b))

save(f3,file = 'Rdata/f3_mfrho2.Rdata')