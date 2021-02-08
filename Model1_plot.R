setwd('C:/Users/huihu/Desktop/2021美赛')
getwd()
library('ggplot2')
data=read.csv('Q2_data.csv')
ggplot(data=data,aes(y=name,x=ranking))+
  geom_point()+
  labs(y='fungi names',x='competition ranking')
