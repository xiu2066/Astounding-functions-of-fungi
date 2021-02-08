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
moisture=-0.5
temperature=seq(0,50,0.1)
decomposition_rate=rep(0,length(temperature))
for(i in 1:length(temperature)){
  decomposition_rate[i]=v(temperature[i],moisture)
}
max_rate=max(decomposition_rate)
temperature1=rep(temperature,2)
decomposition_rate1=c(decomposition_rate,rep(max_rate/2,length(temperature)))
col=c(rep("decomposition rate",length(temperature)),rep("half of maximum rate",length(temperature)))
data1=data.frame(temperature1,decomposition_rate,col)
ggplot(data=data1,aes(x=temperature1,y=decomposition_rate1,col=col))+
  geom_line()+
  labs(x="temperature",y="decomposition rate")

temperature=22
moisture=seq(-1,0,0.01)
decomposition_rate=rep(0,length(moisture))
for(i in 1:length(moisture)){
  decomposition_rate[i]=v(temperature,moisture[i])
}
max_rate=max(decomposition_rate)
moisture1=rep(moisture,2)
decomposition_rate1=c(decomposition_rate,rep(max_rate/2,length(moisture)))
col=c(rep("decomposition rate",length(moisture)),rep("half of maximum rate",length(moisture)))
data1=data.frame(moisture1,decomposition_rate,col)
ggplot(data=data1,aes(x=moisture1,y=decomposition_rate1,col=col))+
  geom_line()+
  labs(x="moisture",y="decomposition rate")