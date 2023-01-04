source('Fijo/parametros fijos.R')
source('Fijo/func sim.R')
b <- 500
mc <- 1000

rho <- c(-0.7,-0.3,0.3,0.7)
######Para muestras de 50 curvas
p50d <- list()
m50d <- list()

pb <- txtProgressBar(min = 0,      # Valor mínimo de la barra de progreso
                     max = length(rho), # Valor máximo de la barra de progreso
                     style = 3,    # Estilo de la barra (también style = 1 y style = 2)
                     width = 50,   # Ancho de la barra. Por defecto: getOption("width")
                     char = "=")  

for(k in 1:length(rho)){
  
  aL2n <- c() #L2n
  aL2d <- c() #L2e
  aDe <- c() #De
  afda.usc <- c() #fda.usc
  d50r <- list()
  for (i in 1:mc)
    
  {
    set.seed(i)
    d50r[[i]] <- fdata(func.sim(n=n1,mdata = length(t),mu = m1(t),sigma = S,rho = rho[k]),argvals = argvals)
    m50d[[k]] <- d50r
    save(m50d,file='Rdata/muestra50D.Rdata')
    f1 <- fLOCI(d50r[[i]],alpha =  alpha,nn = n1/2,dist='L2')
    f2 <- fLOCI(d50r[[i]],alpha =  alpha,nn = n1/2,dist='L2e')
    a <- depth.R1(t(d50r[[i]]$data))
    f3 <- elastic_outliers(a)
    f4 <- outliers.depth.trim(d50r[[i]],nb=b)
    
    aL2n <- cbind(aL2n,sum(f1$class=="Outlier")/n1)
    aL2d <- cbind(aL2d,sum(f2$class=="Outlier")/n1)
    aDe <- cbind(aDe,sum(f3[[1]])/n1)
    afda.usc <- cbind(afda.usc,length(f4$outliers)/n1)
  }
  
  p50d[[k]] <- rbind(aL2n,aL2d,aDe,afda.usc)
  
  setTxtProgressBar(pb, i)
  
}
result <- rbind(aL2n,aL2d,aDe,afda.usc)
close(pb) # Cerramos la conexión
save(p50d,file = 'Rdata/p50d_error1.RData')
