source('Fijo/parametros fijos.R')
source('Fijo/func sim.R')

b <- 500
mc <- 1000


# mfrho3 <- list()
# for(j in 1:length(m_f)){
#   for(i in 1:mc) {
#     set.seed(i)
#     mfrho3[[i]] <-  fdata(func.sim(n=no,mdata = length(t),mu = m1(t),sigma = S,rho = rho[3],mu2 = m_f[[j]],m=o),
#                            argvals = argvals)
#   }
# }
# 
# save(mfrho3,file = 'Rdata/mfrho3.Rdata')

load('Rdata/mfrho3.Rdata')


f1 <- lapply(mfrho3,function(x) fLOCI(x,alpha =  alpha,nn = n/2,dist='L2'))
f2 <- lapply(mfrho3,function(x) fLOCI(x,alpha =  alpha,nn = n/2,dist='L2e'))
#a <- lapply(mfrho3,function(x) depth.R1(t(x$data)))
#f3 <- lapply(a,function(x) elastic_outliers(x))
f4 <- lapply(mfrho3,function(x) outliers.depth.trim(x,nb=b))


result <- list(f1,f2,f4)
save(result,file = 'Rdata/result_mfrho3.Rdata')