#####Para muestra50s de 50 curvas
source('Fijo/parametros fijos.R')
b <- 500
mc <- 1000

aL2n <- c() #L2n
aL2d <- c() #L2e
aDe <- c() #De
afda.usc <- c() #fda.usc
muestra50 <- list()

pb <- txtProgressBar(min = 0,      # Valor mínimo de la barra de progreso
                     max = mc, # Valor máximo de la barra de progreso
                     style = 3,    # Estilo de la barra (también style = 1 y style = 2)
                     width = 50,   # Ancho de la barra. Por defecto: getOption("width")
                     char = "=")  
for (i in 1:mc)
  
{
  set.seed(i)
  muestra50[[i]] <- fdata(mvrnorm(n1, m1(t), S),argvals = argvals)
  save(muestra50,file='Rdata/muestra50I.Rdata')
  f1 <- fLOCI(muestra50[[i]],alpha =  alpha,nn = n1/2,dist='L2')
  f2 <- fLOCI(muestra50[[i]],alpha =  alpha,nn = n1/2,dist='L2e')
  a <- depth.R1(t(muestra50[[i]]$data))
  f3 <- elastic_outliers(a)
  f4 <- outliers.depth.trim(muestra50[[i]],nb=b)
  
  aL2n <- cbind(aL2n,sum(f1$class=="Outlier")/n1)
  aL2d <- cbind(aL2d,sum(f2$class=="Outlier")/n1)
  aDe <- cbind(aDe,sum(f3[[1]])/n1)
  afda.usc <- cbind(afda.usc,length(f4$outliers)/n1)
  
  setTxtProgressBar(pb, i)
}
result <- rbind(aL2n,aL2d,aDe,afda.usc)

close(pb) # Cerramos la conexión
#save.image(result, file = "Rdata/In50.RData")