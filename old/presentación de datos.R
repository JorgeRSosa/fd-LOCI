library(readxl)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(reshape) 
library(lubridate) #Para trabajar las fechas
library(openxlsx)
####################Cargar datos####################

data <- read.xlsx('Datos.xlsx',sheet = 'Humedad')

################################################################################
################# 50 primeros d?as del a?o 2014 ################################
################################################################################
df <- data[1:365,]
h <- 1:24
df <- cbind(h,t(df))
df <- as.data.frame(df) #Data Frame de Humedad Promedio

df1 <- df[,c(1:50)]
#df2 <- d[,c(1:50)]
is.na(df)


# #### Grafico por dia de la Humedad Promedio
df_ <-  melt(df1,id.vars = 'h')

ggplot(df_, aes(x = h, y = value, col=variable, group=variable)) +
  theme( axis.text.x = element_blank ( ) ) +
  labs(x='Hour',y='Average Humidity') + 
  geom_line()

##################################################
###########Pasar a Datos Funcionales#############
################################################
library(fda)
library(fda.usc)
library(ftsa)


## Transformar a objeto fdata ( functional data )

argvals <- 1:24
dff <- fdata ( data , argvals = argvals ,
                 names = list(main= 'Humedad Promedio' ,
                                  xlab= 'Hora del Dia ' , ylab= 'Humedad Promedio') )
is.fdata ( dff )
dim( dff )
rangeval ( dff )

# Buscar K y lambda , adecuados para e l SUAVIZAMIENTO
# nb <- floor(seq(5,29, len=10))
# length ( nb )
# 
# l<-2^(-5:15)
# length(l)
# 
# opt <- optim.basis( dff , lambda = l , numbasis = nb ,
#                           type.CV = GCV.S , type.basis="fourier" )
# 
# 
# save(opt,file = 'opt.RData')
load('opt.RData')
min( opt$gcv )
K <- opt$numbasis.opt
lambda <- opt$lambda.opt
## De los pares de (K, l ) se observa e l menor GCV
gcv <-  as.data.frame ( opt$gcv )
colnames ( gcv ) <-  round ( as.numeric ( colnames ( gcv ) ) , 2)
gcv$nb <- as.numeric ( rownames ( gcv ) ) ## creo nueva columna
ind1 <- melt ( gcv , id.vars = 'nb' )
ind1 %>% ggplot ( aes ( x = nb , y = value , col = variable , group = variable ) ) +
  labs ( x = "Numero Bases " , y = 'GCV' ) +
  geom_point ( )

# #####################
## Ajus te de los datos
# #####################

# dataf <-  dff
# dataf$data <- dff$data%*%opt$S.opt # producto X* S
# save(dataf,file = 'dataf.Rdata')
load('dataf.Rdata')

plot(dataf , main=" Suavizamiento con Fourirer ",)
is.fdata(dataf)


############################################################
########## Grafica de un dia sin suavizar y un dia suavizado 
###### primer dia del 2014
a <- unlist(data[1,])

plot(a,xlab = 'Time (hours)',ylab = 'Average Humidity (%)')
lines(dataf$data[1,],type = 'l',col='red')



