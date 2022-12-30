library(tidyverse)
library(fda)
library(fda.usc)
library(MASS)
library(ggplot2)
library(openxlsx)
source("LOCI.R")
select<-dplyr::select
fechas_et <- read.xlsx('Datos.xlsx',sheet = 'Fechas')
load('dataf.Rdata')

##### encontrar el theta mas peque?o que tome mas anomal[ias ################
theta <- seq(0.7,1,0.04)

# clase<-c()
#  for(k in 1:length(theta)){
#    clase<-cbind(clase,fLOCI(dataf,theta[k])$class)
#  }
#  colnames(clase)<-paste0("theta ",theta)
#  clase<-data.frame(clase)
# 
# 
# save(clase,file="clase_toda_data.RData")
load("clase_toda_data.RData")
apply(clase,2,function(x)sum(x=="Outlier"))
#me quedo con 0.78
################################establecemos la cantidad de radios########################

radios<-round(seq(20,nrow(dataf$data),length.out = 100),0)
radios[101] <- 1095
radios[102] <- 1533
# score<- c()
#  for (i in 1:length(radios) ){
#    a<- fLOCI(dataf,0.78,radios[i])
#    score<-cbind(score,a$MDEF/a$norm_MDEF)
#  }
# score<-data.frame(score)
# names(score)<-paste0("r_",radios)
# ##########################################Selecciono el maximo valor del mdef##############
#save(score,file="matriz_score_td.Rdata")
load("matriz_score_td.Rdata")
############################################Selecciono el radio con mejor desempeño#############
clase<-ifelse(score[,1]>3,1,0)
for (i in 2:ncol(score)) {
  clase<-cbind(clase,ifelse(score[,i]>3,1,0))
}
clase<-data.frame(clase)
names(clase)<-paste0('r_',radios)

clase<-cbind(fechas_et[,1:2],clase)

####################################################################
#Por cada radio sacamos las anomal?as
anomalias<-list()

for(i in 3:ncol(clase)){
  anomalias[[i-2]]<-clase[,c(1,i)] [clase[,c(i)]==1,] 
}
##################################################################
anom<-list()
for(i in 1:length(anomalias)){
  anom[[i]]<- fechas_et %>% mutate(evaluacion=ifelse(Fecha%in%anomalias[[i]]$Fecha,1,0))
}
names(anom)<-radios
mc<-lapply(anom, function(x)table(x$evaluacion,x$Outlier))
acc<-lapply(mc,function(x)(x[1,1]+x[2,2])/sum(x))
pres<-lapply(mc, function(x)(x[1,1] /(x[1,1] +x[1,2])))
acc<-unlist(acc)
pres<-unlist(pres)
which(acc%in%max(acc))
which(pres%in%max(pres))
mc[[46]]
acc[46]
pres[46]
radios[which(acc%in%max(acc))]

anom[[46]] <- anom[[46]] %>% mutate(Outlier=as.numeric(Outlier))

library(plotROC)
p<- ggplot(anom[[46]], aes(d=Outlier,m=evaluacion)) + geom_roc()+ style_roc() #Grafica Curva ROC basica para colocar el area bajo la curva
#Curva ROC
logroc<-ggplot(anom[[46]], aes(d=Outlier,m=evaluacion)) +
  theme_bw()+
  geom_roc(n.cuts = 0, colour="#3AA717") + 
  theme(axis.text = element_text(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Curva roc LOCI limite 3 ")+
  scale_x_continuous("\n1 - Especifidad (FPF)", breaks = seq(0, 1, by = .2))+
  scale_y_continuous("Sensibilidad (TPF)\n", breaks = seq(0, 1, by = .2)) +
  geom_abline(intercept=0, slope=1, colour="blue", linetype="dashed") +
  annotate("text", x=0.6, y=0.45, parse=TRUE,
           label=paste0("AUC: ",round(calc_auc(p)$AUC,3)), colour="blue")+
  ggExtra::removeGridX()+
  ggExtra::removeGridY()

logroc
#AUC=0.813



############################################################
######### grafico de la base de datos con atipicos detectados por FLOCI
fechas_et$OFLOCI <- ifelse(anom[[46]]$evaluacion,1,0)
fechas_et$OND <- ifelse(fechas_et$Outlier==1,ifelse(fechas_et$OFLOCI==0,1,0),0) # outlier no detectados
fechas_et$OCD <- ifelse(fechas_et$Outlier==1,ifelse(fechas_et$OFLOCI==1,1,0),0) # outlier correctamente detectado


plot ( dataf , col='grey',ylim=c(-1,101),xlab = 'Time (hours)',ylab = 'Average Humidity (%)') 
lines(dataf[fechas_et$OCD==1,], col='black')
lines(dataf[fechas_et$OND==1,], col='green')
legend("bottomright",
      # dependiendo del tama?o de tu gr?fico
       legend = c("N", "C.D.A","N.D.A"), 
       lwd = 1,
       col = c('grey','black' ,'green'),
       bty = 'n',
       cex = 0.8
       ) 




################################################################################
#########outliers por profundidad funcional ####################################

out <- outliers.depth.trim(dataf,nb=200)
out2 <- outliers.depth.pond(dataf)


load('outlier_depth_trim.rdata')
fechas_et$depthtrim <- ifelse(fechas_et$id%in%out$outliers,1,0)
table(fechas_et$depthtrim,fechas_et$Outlier)

################################################################################
################# outlier por qcr##############################################

#fd <- fdqcd(dataf$data)
#fddep <- fdqcs.depth(fd,plot = T)

#save(fddep,file = 'fddep.RData')
load('fddep.RData')

fechas_et$qcr <- ifelse(fechas_et$id%in%fddep$out,1,0)
table(fechas_et$qcr,fechas_et$Outlier)


fboxplot <- rep(0,length(fechas_et$Outlier))
fboxplot[620] <- 1
fboxplot[1752] <- 1

fechas_et$fboxplot <- fboxplot
#################################################################################
######### matriz de confusión con confusionMatriz

library(caret)

matriz_fdloci <- confusionMatrix(as.factor(anom[[46]]$Outlier),as.factor(anom[[46]]$evaluacion))
matriz_qcr <- confusionMatrix(as.factor(fechas_et$Outlier),as.factor(fechas_et$qcr))
matriz_fboxplot <- confusionMatrix(as.factor(fechas_et$Outlier),as.factor(fechas_et$fboxplot))
