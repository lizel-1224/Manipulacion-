setwd("D:/Cuarto Ciclo/Programación")
read.csv("datos.csv")
dts<-read.csv("datos.csv")
dlc<-as.matrix(dts[,2:13])
dlc<-t(dlc)
dlc<-matrix(dlc, ncol = 1)
df<-data.frame(A=rep(1975:2013, rep(12,39)), as.data.frame(dlc))
names(df)<-c("año","PP")

md<-mean(df$PP)
dvs<-sd(df$PP)
qtl<-quantile(df$PP, probs = c(0.25,0.75))
rangointercuartil<-qtl[2] - qtl[1]
plot(df$PP)
paso<-2*rangointercuartil
CEi<-qtl[2]+paso
CEs<-qtl[2]+2*paso

plot(df$PP, col=ifelse(df$PP > CEs, "red", "gray"),
     pch=ifelse(df$PP>CEs, 8, 19),xlab="tiempo", ylab="precipitacion(mm)")
abline(h=c(CEi,CEs,md) ,col = c("blue", "red" ,"black") , lty=2)

dtz<-data.frame(z = (df$PP - md)/dvs)
plot(dtz$z,col=ifelse(dtz$z > 3, "red", "gray") ,
     pch= ifelse(dtz$z> 3, 8, 19) , xlab= "tiempo" , ylab="precipitacion(mm)")
abline(h=3 , col="red" , lty=2)

boxplot(dts[,2:13] , xlab="meses" , ylab="precipitacion(mm)", 
        col= c(rainbow(12)), horizontal = FALSE , boxwex = 0.6 , border="blue")

hist(df$PP)
qqnorm(df$PP , pch = 1)
qqline(df$PP, col="green" , lty=2 )
       