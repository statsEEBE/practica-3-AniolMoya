#Solucion Pregunta 1
x<-c(0,1)
fx<-c(0.68,0.32)  

#tabla prob.
cbind(x,fx)
mu<- sum(x*fx)
Vx<-(0.32*(1-0.32))

(0.32*44)/17
n<-43
Y<- function(i){sum(sample(x , n, prob=fx, replace=TRUE))}
#bucle
m<-4000000
muestra<- sapply(1:m, Y)
fi<-table(muestra)/m
fi
data.frame(fi, Fi=cumsum(fi))
barplot(fi)
data.frame(y=0:43, Prob=dbinom(y,43,0.32))
fy<-dbinom(y,43,0.32)
y=0:43
plot( y, fy, pch=19, col="blue")
lines(y,fy,col="red",type="h")
##########################################
#apartat 2
y<- 0:44
df<- data.frame()
Pi <- dbinom(y,44,0.32)
Fi<- cumsum(Pi)

cbind(y,Pi,Fi)

plot(y, Fi,type="s", col="red")
###############################
#Apartat3
x<-0:24
Pi<-dbinom(x,24,0.68)
n<-24
mu<-sum(x*Pi)
mu
Vx<-sum((x-mu)^2*Pi)
Vx
q25<-qbinom(0.25,24,0.68)#valor al que Fi=0.25
Fi<- cumsum(Pi)
plot(x, Fi,type="s", col="red")
q25
######
