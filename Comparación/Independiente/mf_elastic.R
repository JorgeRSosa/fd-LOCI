source('Fijo/parametros fijos.R')
b <- 500
mc <- 1000


# mf <- list()
# for(j in 1:length(m_f)){
#   for(i in 1:mc) {
#     set.seed(i)
#     mf[[i]] <- fdata(rbind(mvrnorm(no, m1(t), S),mvrnorm(o, m_f[[j]], S)),argvals = argvals) 
#   }
# }
# 
# save(mf,file = 'Rdata/mfI.Rdata')

load('Rdata/mfI.Rdata')


#f1 <- lapply(mf,function(x) fLOCI(x,alpha =  alpha,nn = n/2,dist='L2'))
#f2 <- lapply(mf,function(x) fLOCI(x,alpha =  alpha,nn = n/2,dist='L2e'))
a <- lapply(mf,function(x) depth.R1(t(x$data)))
f3 <- lapply(a,function(x) elastic_outliers(x))
#f4 <- lapply(mf,function(x) outliers.depth.trim(x,nb=b))


save(f3,file = 'Rdata/f3_mfI.Rdata')