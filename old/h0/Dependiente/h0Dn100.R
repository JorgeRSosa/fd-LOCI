source('Fijo/parametros fijos.R')
source('Fijo/func sim.R')
b <- 500
mc <- 1000

rho <- c(-0.7,-0.3,0.3,0.7)

######Para muestras de 100 curvas
p100d <- list()
m100d <- list()
pb <- txtProgressBar(min = 0,      # Valor mínimo de la barra de progreso
                     max = length(rho), # Valor máximo de la barra de progreso
                     style = 3,    # Estilo de la barra (también style = 1 y style = 2)
                     width = 50,   # Ancho de la barra. Por defecto: getOption("width")
                     char = "=") 

for(k in 1:length(rho)){
  aL2n <- c()
  aL2d <- c()
  aDe <- c()
  afda.usc <- c()
  d100r <- list()
  for (i in 1:mc){
    
    set.seed(i)
    d100r[[i]] <- fdata(func.sim(n=n2,mdata = length(t),mu = m1(t),sigma = S,rho = rho[k]),argvals = argvals)
    m100d[[k]] <- d100r
    save(m100d,file='Rdata/muestra100D.Rdata')
    f1 <- fLOCI(d100r[[i]],alpha =  alpha,nn = n2/2,dist='L2')
    f2 <- fLOCI(d100r[[i]],alpha =  alpha,nn = n2/2,dist='L2e')
    a <- depth.R1(t(d100r[[i]]$data))
    f3 <- elastic_outliers(a)
    f4 <- outliers.depth.trim(d100r[[i]],nb=b)
    
    aL2n <- cbind(aL2n,sum(f1$class=="Outlier")/n2)
    aL2d <- cbind(aL2d,sum(f2$class=="Outlier")/n2)
    aDe <- cbind(aDe,sum(f3[[1]])/n2)
    afda.usc <- cbind(afda.usc,length(f4$outliers)/n2)
  }
  
  p100d[[k]] <- rbind(aL2n,aL2d,aDe,afda.usc)
  setTxtProgressBar(pb, i)
     
}
result <- rbind(aL2n,aL2d,aDe,afda.usc)
close(pb) # Cerramos la conexión
save(p100d,file = 'Rdata/p100d_error1.RData')
