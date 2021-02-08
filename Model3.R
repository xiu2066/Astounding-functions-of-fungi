setwd('C:/Users/huihu/Desktop/2021美赛')
getwd()
library("ggplot2")
library("ggpubr")
library("ggrepel")
library("pheatmap")
data=read.csv('Q4_data.csv')
E=rep(1,34)
t=0
a=0
b=0
m=0
c=0
d=0
e=0
f=0
r=0
x=log(c(data[,'e.rate']))
y=log(c(data[,'d.rate.geo']))
relation=lm(y~x)
A=relation$coefficients['x']
B=relation$coefficients['(Intercept)']
for(i in 1:34){
  m[i]=data[i,'water.mpa.at.max.r']
  c[i]=data[i,'water.max.r']
  d[i]=c[i]/2/(-data[i,'water.niche.low']-m[i])^2
  e[i]=c[i]/2/(-data[i,'water.high']-m[i])^2
  t[i]=data[i,'temp.temp.at.max.rate']
  f[i]=exp(A*log(c[i])+B)
  a[i]=(data[i,'d.rate10']-data[i,'d.rate22'])/((22-t[i])^2-(10-t[i])^2)
  b[i]=data[i,'d.rate10']-f[i]+a[i]*(10-t[i])^2
  r[i]=data[i,'ranking']
}

f1<-function(j,t1){
  result=-a[j]*(t1-t[j])^2+b[j]
  return(result)
}

f2<-function(j,m1){
  result=0
  if(m1<m[j])
    result=-d[j]*(m1-m[j])^2+c[j]
  else
    result=-e[j]*(m1-m[j])^2+c[j]
  if(result>0.01){
    h=exp(A*log(result)+B)
  }
  else{
    h=0
  }
  return(h)
}
alpha<-function(j,t1,m1){
  u=(0.1+r[j])*(1e-20+max(0,f1(j,t1)+f2(j,m1)))/(1e-20+max(0,f1(j,t[j])+f2(j,m[j])))
  return(u)
}
v<-function(t1,m1){
  s=0
  l=0
  for(j in 1:34){
    s=s+E[j]*alpha(j,t1,m1)*max(0,(f1(j,t1)+f2(j,m1)))
    l=l+E[j]*alpha(j,t1,m1)
  }
  result=s/l
  return(result)
}

#3.1.3
moisture=-0.5
temperature=seq(0,50,0.1)
l=length(temperature)
temperature=rep(temperature,34)
col=rep(data[1,'name'],l)
for(i in 2:34){
  col=c(col,rep(data[i,'name'],l))
}
decomposition_rate=0
for(i in 1:34){
  for(j in 1:l){
    decomposition_rate[j+(i-1)*l]=max(0,f1(i,temperature[j])+f2(i,moisture))
  }
}
res=data.frame(decomposition_rate,temperature,col)
ggplot(res,aes(x=temperature,y=decomposition_rate,col=col))+
  geom_line()

#5.4.1
moisture=-0.5
temperature=seq(0,50,0.1)
decomposition_rate=rep(0,length(temperature))
for(i in 1:length(temperature)){
  decomposition_rate[i]=v(temperature[i],moisture)
}
p1=ggplot(data=data.frame(temperature,decomposition_rate),aes(x=temperature,y=decomposition_rate))+
  geom_line(col='blue')+
  ylim(0,30)
E=rep(0,34)
for(i in 1:17){
  E[i]=1
}
moisture=-0.5
temperature=seq(0,50,0.1)
decomposition_rate=rep(0,length(temperature))
for(i in 1:length(temperature)){
  decomposition_rate[i]=v(temperature[i],moisture)
}
p2=ggplot(data=data.frame(temperature,decomposition_rate),aes(x=temperature,y=decomposition_rate))+
  geom_line(col='green')+
  ylim(0,30)
E=rep(0,34)
for(i in 1:5){
  E[i]=1
}
moisture=-0.5
temperature=seq(0,50,0.1)
decomposition_rate=rep(0,length(temperature))
for(i in 1:length(temperature)){
  decomposition_rate[i]=v(temperature[i],moisture)
}
p3=ggplot(data=data.frame(temperature,decomposition_rate),aes(x=temperature,y=decomposition_rate))+
  geom_line(col='red')+
  ylim(0,30)
ggarrange(p1,p2,p3,nrow=3)
E=rep(1,34)
moisture=seq(-1,0,0.01)
temperature=22
decomposition_rate=rep(0,length(moisture))
for(i in 1:length(moisture)){
  decomposition_rate[i]=v(temperature,moisture[i])
}
p1=ggplot(data=data.frame(moisture,decomposition_rate),aes(x=moisture,y=decomposition_rate))+
  geom_line(col='blue')+
  ylim(12,24)
E=rep(0,34)
for(i in 1:17){
  E[i]=1
}
moisture=seq(-1,0,0.01)
temperature=22
decomposition_rate=rep(0,length(moisture))
for(i in 1:length(moisture)){
  decomposition_rate[i]=v(temperature,moisture[i])
}
p2=ggplot(data=data.frame(moisture,decomposition_rate),aes(x=moisture,y=decomposition_rate))+
  geom_line(col='green')+
  ylim(12,24)
E=rep(0,34)
for(i in 1:5){
  E[i]=1
}
moisture=seq(-1,0,0.01)
temperature=22
decomposition_rate=rep(0,length(moisture))
for(i in 1:length(moisture)){
  decomposition_rate[i]=v(temperature,moisture[i])
}
p3=ggplot(data=data.frame(moisture,decomposition_rate),aes(x=moisture,y=decomposition_rate))+
  geom_line(col='red')+
  ylim(12,24)
ggarrange(p1,p2,p3,nrow=3)
#5.4.2
ggplot(data=data,aes(x=temp.temp.at.max.rate,y=water.mpa.at.max.r,col=name))+
  geom_point()+
  geom_text_repel(aes(label=X))+
  labs(x='Optimal temperature',y='Optimal moisure')

moisture=seq(-0.4,-0.2,0.005)
temperature=seq(20,30,0.02)
E=rep(0,34)
E[c(19,20,29,30,33)]=1
decomposition_rate=matrix(rep(0,length(moisture)*length(temperature)),length(moisture),length(temperature))
for(i in 1:length(moisture)){
  for(j in 1:length(temperature)){
    decomposition_rate[i,j]=v(temperature[j],moisture[i])
  }
}
colnames(decomposition_rate)=c(paste("temperature=",temperature[1]),
                               rep(' ',length(temperature)-2),
                               paste("temperature=",temperature[length(temperature)]))
rownames(decomposition_rate)=c(paste('moisture=',moisture[1]),
                               rep(' ',length(moisture)-2),
                               paste("moisture=",moisture[length(moisture)]))
pheatmap(decomposition_rate,cluster_rows = F,cluster_cols = F)
E=rep(0,34)
E[c(3,4,7,11,16)]=1
decomposition_rate=matrix(rep(0,length(moisture)*length(temperature)),length(moisture),length(temperature))
for(i in 1:length(moisture)){
  for(j in 1:length(temperature)){
    decomposition_rate[i,j]=v(temperature[j],moisture[i])
  }
}
colnames(decomposition_rate)=c(paste("temperature=",temperature[1]),
                               rep(' ',length(temperature)-2),
                               paste("temperature=",temperature[length(temperature)]))
rownames(decomposition_rate)=c(paste('moisture=',moisture[1]),
                               rep(' ',length(moisture)-2),
                               paste("moisture=",moisture[length(moisture)]))
pheatmap(decomposition_rate,cluster_rows = F,cluster_cols = F)
