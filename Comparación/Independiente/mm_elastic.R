source('Fijo/parametros fijos.R')
b <- 500
mc <- 1000


# mm <- list()
# for(j in 1:length(m_m)){
#   for(i in 1:mc) {
#     set.seed(i)
#     mm[[i]] <- fdata(rbind(mvrnorm(no, m1(t), S),mvrnorm(o, m_m[[j]], S)),argvals = argvals) 
#   }
# }
# 
# save(mm,file = 'Rdata/mmI.Rdata')

load('Rdata/mmI.Rdata')


#f1 <- lapply(mm,function(x) fLOCI(x,alpha =  alpha,nn = n/2,dist='L2'))
#f2 <- lapply(mm,function(x) fLOCI(x,alpha =  alpha,nn = n/2,dist='L2e'))
a <- lapply(mm,function(x) depth.R1(t(x$data)))
f3 <- lapply(a,function(x) elastic_outliers(x))
#f4 <- lapply(mm,function(x) outliers.depth.trim(x,nb=b))


save(f3,file = 'Rdata/f3_mmI.Rdata')