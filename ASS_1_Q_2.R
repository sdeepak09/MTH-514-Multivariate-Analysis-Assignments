## Assignment 2 Answer of question 2
dt=read.csv("q2_data.csv")
View(dt)
dt=dt[,-21]
bank_vec=dt[,1]
dt_new=dt[,-1]
View(dt_new)
sum(is.na(dt_new)) ## as Sum is 0 so we can say that there is no missing values in the data 
scaled_dt=scale(dt_new)
cov_mt=t(scaled_dt)%*%(scaled_dt)
cor_mt=cov_mt/cov_mt[1,1]
eig_vl_vc=eigen(cor_mt)
eig_vl=eig_vl_vc$values
eig_vc=eig_vl_vc$vectors
proj_dt=as.matrix(scaled_dt)%*%eig_vc[,1:3]

## 1st Part
xx=c(1:27)
yy=matrix(rep(xx,4),byrow = T, nrow = 4)
zz=as.vector(yy)
zz
library(rgl)
plot3d(proj_dt[which(zz==1),],type = 's',xlab = "PC1",ylab = "PC2",zlab =  "PC3",radius = 0.3)
plot3d(proj_dt,col=as.factor(zz),type = 's',xlab = "PC1",ylab = "PC2",zlab =  "PC3",radius = 0.3)
xxx=c(1:4)
yyy=matrix(rep(xxx,27),byrow = T, nrow = 27)
zzz=as.vector(t(yyy))
plot3d(proj_dt,col=as.factor(zzz),type = 's',xlab = "PC1",ylab = "PC2",zlab =  "PC3",radius = 0.3)


## Multidimensional outliers
plot3d(proj_dt,type = 's',xlab = "PC1",ylab = "PC2",zlab =  "PC3",radius = 0.03)
text3d(proj_dt,texts = as.factor(c(1:nrow(proj_dt))),col="red")

## 29,30,31,32,37,38,39,40,45,46,47,48,69,70,71,72,97,98,99,100 

dt_wo_outlr=dt_new[-c(29,30,31,32,37,38,39,40,45,46,47,48,69,70,71,72,97,98,99,100),]
scald_dt_wo_outlr=scale(dt_wo_outlr)
cov_mt_1=t(scald_dt_wo_outlr)%*%(scald_dt_wo_outlr)
cor_mt_1=cov_mt_1/cov_mt_1[1,1]
eig_vl_vc_1=eigen(cor_mt_1)
eig_vl_1=eig_vl_vc_1$values
eig_vc_1=eig_vl_vc_1$vectors
proj_dt_1=as.matrix(scald_dt_wo_outlr)%*%eig_vc_1[,1:3]
plot3d(proj_dt_1,type = 's',xlab = "PC1",ylab = "PC2",zlab =  "PC3",radius = 0.3)


