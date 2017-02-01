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
plot3d(proj_dt,col=as.factor(zz),type = 's',xlab = "PC1",ylab = "PC2",zlab =  "PC3",radius = 0.3)
plot3d(proj_dt[which(zz==1),],type = 'l',xlab = "PC1",ylab = "PC2",zlab =  "PC3",radius = 0.1)
text3d(proj_dt[which(zz==1),],texts = bank_vec[1:4])
#red=96-97  blue=97-98  green=98-99  pink=1999-2000
xxx=c(1:4)
yyy=matrix(rep(xxx,27),byrow = T, nrow = 27)
zzz=as.vector(t(yyy))
plot3d(proj_dt,col=as.factor(zzz),type = 's',xlab = "PC1",ylab = "PC2",zlab =  "PC3",radius = 0.3)





## Multidimensional outliers
# for financial year 1996-1997
tag=rep(c(1:4),27)
bank_names=bank_vec[which(tag==1)]
dt_96_97=dt_new[which(tag==1),]
dim(dt_96_97)
dt_97_98=dt_new[which(tag==2),]
dim(dt_97_98)
dt_98_99=dt_new[which(tag==3),]
dim(dt_98_99)
dt_99_00=dt_new[which(tag==4),]
dim(dt_99_00)

## Scaling and projection for 96_97
scaled_dt_96_97=scale(dt_96_97)
cov_mt_96_97=t(scaled_dt_96_97)%*%(scaled_dt_96_97)
cor_mt_96_97=cov_mt_96_97/cov_mt_96_97[1,1]
eig_vl_vc_96_97=eigen(cor_mt_96_97)
eig_vl_96_97=eig_vl_vc_96_97$values
eig_vc_96_97=eig_vl_vc_96_97$vectors
proj_dt_96_97=as.matrix(scaled_dt_96_97)%*%eig_vc_96_97[,1:3]
plot3d(proj_dt_96_97,type = 's',xlab = "PC1",ylab = "PC2",zlab =  "PC3",radius = 0.1)
text3d(proj_dt_96_97,texts = as.factor(c(1:nrow(scaled_dt_96_97))))

## 10 is outlier
dt_wo_outlr_96_97=dt_96_97[-10,]
scaled_dt_96_97_wo_outlr=scale(dt_wo_outlr_96_97)
cov_mt_96_97_wo_outlr=t(scaled_dt_96_97_wo_outlr)%*%(scaled_dt_96_97_wo_outlr)
cor_mt_96_97_wo_outlr=cov_mt_96_97_wo_outlr/cov_mt_96_97_wo_outlr[1,1]
eig_vl_vc_96_97_wo_outlr=eigen(cor_mt_96_97_wo_outlr)
eig_vl_96_97_wo_outlr=eig_vl_vc_96_97_wo_outlr$values
eig_vc_96_97_wo_outlr=eig_vl_vc_96_97_wo_outlr$vectors
proj_dt_96_97_wo_outlr=as.matrix(scaled_dt_96_97_wo_outlr)%*%eig_vc_96_97_wo_outlr[,1:3]
plot3d(proj_dt_96_97_wo_outlr,type = 's',xlab = "PC1",ylab = "PC2",zlab =  "PC3",radius = 0.1)
text3d(proj_dt_96_97_wo_outlr,texts = as.factor(c(1:nrow(scaled_dt_96_97_wo_outlr))))

## Ranking
banks_96_97=bank_names[-10]
PC1_96_97=proj_dt_96_97_wo_outlr[,1]
bank_PC1_96_97=data.frame(banks_96_97,PC1_96_97)
eig_vc_96_97_wo_outlr[,1]
## From above we can see that for most of the +ve factors coefficient is negative so that we will consider the lower the best technique
bank_PC1_96_97=bank_PC1_96_97[order(bank_PC1_96_97[,2]),]




## Clustering 
clust_96_97=kmeans(proj_dt_96_97_wo_outlr,6, iter.max = 100,nstart = 3)
cl_96_97=clust_96_97$cluster
plot3d(proj_dt_96_97_wo_outlr,type = 's',col=cl_96_97,xlab = "PCA1",ylab = "PCA2",zlab =  "PCA3",radius = 0.3)





## Scaling and projection for 97_98
scaled_dt_97_98=scale(dt_97_98)
cov_mt_97_98=t(scaled_dt_97_98)%*%(scaled_dt_97_98)
cor_mt_97_98=cov_mt_97_98/cov_mt_97_98[1,1]
eig_vl_vc_97_98=eigen(cor_mt_97_98)
eig_vl_97_98=eig_vl_vc_97_98$values
eig_vc_97_98=eig_vl_vc_97_98$vectors
proj_dt_97_98=as.matrix(scaled_dt_97_98)%*%eig_vc_97_98[,1:3]
plot3d(proj_dt_97_98,type = 's',xlab = "PC1",ylab = "PC2",zlab =  "PC3",radius = 0.1)
text3d(proj_dt_97_98,texts = as.factor(c(1:nrow(scaled_dt_97_98))))

## 10 is outlier
dt_wo_outlr_97_98=dt_97_98[-c(8,10,12,22),]
scaled_dt_97_98_wo_outlr=scale(dt_wo_outlr_97_98)
cov_mt_97_98_wo_outlr=t(scaled_dt_97_98_wo_outlr)%*%(scaled_dt_97_98_wo_outlr)
cor_mt_97_98_wo_outlr=cov_mt_97_98_wo_outlr/cov_mt_97_98_wo_outlr[1,1]
eig_vl_vc_97_98_wo_outlr=eigen(cor_mt_97_98_wo_outlr)
eig_vl_97_98_wo_outlr=eig_vl_vc_97_98_wo_outlr$values
eig_vc_97_98_wo_outlr=eig_vl_vc_97_98_wo_outlr$vectors
proj_dt_97_98_wo_outlr=as.matrix(scaled_dt_97_98_wo_outlr)%*%eig_vc_97_98_wo_outlr[,1:3]
plot3d(proj_dt_97_98_wo_outlr,type = 's',xlab = "PC1",ylab = "PC2",zlab =  "PC3",radius = 0.1)
text3d(proj_dt_97_98_wo_outlr,texts = as.factor(c(1:nrow(scaled_dt_97_98_wo_outlr))))

## Ranking
banks_97_98=bank_names[-c(8,10,12,22)]
PC1_97_98=proj_dt_97_98_wo_outlr[,1]
bank_PC1_97_98=data.frame(banks_97_98,PC1_97_98)
eig_vc_97_98_wo_outlr[,1]
## From above we can see that for most of the +ve factors coefficient is negative so that we will consider the lower the best technique
bank_PC1_97_98=bank_PC1_97_98[order(bank_PC1_97_98[,2]),]


## Clustering 
clust_97_98=kmeans(proj_dt_97_98_wo_outlr,5, iter.max = 100,nstart = 3)
cl_97_98=clust_97_98$cluster
plot3d(proj_dt_97_98_wo_outlr,type = 's',col=cl_97_98,xlab = "PCA1",ylab = "PCA2",zlab =  "PCA3",radius = 0.3)







## Scaling and projection for 98_99
scaled_dt_98_99=scale(dt_98_99)
cov_mt_98_99=t(scaled_dt_98_99)%*%(scaled_dt_98_99)
cor_mt_98_99=cov_mt_98_99/cov_mt_98_99[1,1]
eig_vl_vc_98_99=eigen(cor_mt_98_99)
eig_vl_98_99=eig_vl_vc_98_99$values
eig_vc_98_99=eig_vl_vc_98_99$vectors
proj_dt_98_99=as.matrix(scaled_dt_98_99)%*%eig_vc_98_99[,1:3]
plot3d(proj_dt_98_99,type = 's',xlab = "PC1",ylab = "PC2",zlab =  "PC3",radius = 0.1)
text3d(proj_dt_98_99,texts = as.factor(c(1:nrow(scaled_dt_98_99))))

## 10 is outlier
dt_wo_outlr_98_99=dt_98_99[-c(8,10,12,19,22,25),]
scaled_dt_wo_outlr_98_99=scale(dt_wo_outlr_98_99)
cov_mt_98_99_wo_outlr=t(scaled_dt_wo_outlr_98_99)%*%(scaled_dt_wo_outlr_98_99)
cor_mt_98_99_wo_outlr=cov_mt_98_99_wo_outlr/cov_mt_98_99_wo_outlr[1,1]
eig_vl_vc_98_99_wo_outlr=eigen(cor_mt_98_99_wo_outlr)
eig_vl_98_99_wo_outlr=eig_vl_vc_98_99_wo_outlr$values
eig_vc_98_99_wo_outlr=eig_vl_vc_98_99_wo_outlr$vectors
proj_dt_98_99_wo_outlr=as.matrix(scaled_dt_wo_outlr_98_99)%*%eig_vc_98_99_wo_outlr[,1:3]
plot3d(proj_dt_98_99_wo_outlr,type = 's',xlab = "PC1",ylab = "PC2",zlab =  "PC3",radius = 0.1)
text3d(proj_dt_98_99_wo_outlr,texts = as.factor(c(1:nrow(scaled_dt_wo_outlr_98_99))))

## Ranking
banks_98_99=bank_names[-c(8,10,12,19,22,25)]
PC1_98_99=proj_dt_98_99_wo_outlr[,1]
bank_PC1_98_99=data.frame(banks_98_99,PC1_98_99)
eig_vc_98_99_wo_outlr[,1]
## From above we can see that for most of the +ve factors coefficient is negative so that we will consider the lower the best technique
bank_PC1_98_99=bank_PC1_98_99[order(bank_PC1_98_99[,2]),]


## Clustering 
clust_98_99=kmeans(proj_dt_98_99_wo_outlr,6, iter.max = 100,nstart = 3)
cl_98_99=clust_98_99$cluster
plot3d(proj_dt_98_99_wo_outlr,type = 's',col=cl_98_99,xlab = "PCA1",ylab = "PCA2",zlab =  "PCA3",radius = 0.3)






## Scaling and projection for 99_00
scaled_dt_99_00=scale(dt_99_00)
cov_mt_99_00=t(scaled_dt_99_00)%*%(scaled_dt_99_00)
cor_mt_99_00=cov_mt_99_00/cov_mt_99_00[1,1]
eig_vl_vc_99_00=eigen(cor_mt_99_00)
eig_vl_99_00=eig_vl_vc_99_00$values
eig_vc_99_00=eig_vl_vc_99_00$vectors
proj_dt_99_00=as.matrix(scaled_dt_99_00)%*%eig_vc_99_00[,1:3]
plot3d(proj_dt_99_00,type = 's',xlab = "PC1",ylab = "PC2",zlab =  "PC3",radius = 0.1)
text3d(proj_dt_99_00,texts = as.factor(c(1:nrow(scaled_dt_99_00))))

## 7,8,10,12,24,25 is outlier
dt_wo_outlr_99_00=dt_99_00[-c(7,8,10,12,24,25),]
scaled_dt_wo_outlr_99_00=scale(dt_wo_outlr_99_00)
cov_mt_99_00_wo_outlr=t(scaled_dt_wo_outlr_99_00)%*%(scaled_dt_wo_outlr_99_00)
cor_mt_99_00_wo_outlr=cov_mt_99_00_wo_outlr/cov_mt_99_00_wo_outlr[1,1]
eig_vl_vc_99_00_wo_outlr=eigen(cor_mt_99_00_wo_outlr)
eig_vl_99_00_wo_outlr=eig_vl_vc_99_00_wo_outlr$values
eig_vc_99_00_wo_outlr=eig_vl_vc_99_00_wo_outlr$vectors
proj_dt_99_00_wo_outlr=as.matrix(scaled_dt_wo_outlr_99_00)%*%eig_vc_99_00_wo_outlr[,1:3]
plot3d(proj_dt_99_00_wo_outlr,type = 's',xlab = "PC1",ylab = "PC2",zlab =  "PC3",radius = 0.1)
text3d(proj_dt_99_00_wo_outlr,texts = as.factor(c(1:nrow(scaled_dt_wo_outlr_99_00))))


## Ranking
banks_99_00=bank_names[-c(7,8,10,12,24,25)]
PC1_99_00=proj_dt_99_00_wo_outlr[,1]
bank_PC1_99_00=data.frame(banks_99_00,PC1_99_00)
eig_vc_99_00_wo_outlr[,1]
## From above we can see that for most of the +ve factors coefficient is negative so that we will consider the lower the best technique
bank_PC1_99_00=bank_PC1_99_00[order(bank_PC1_99_00[,2]),]



## Clustering 
clust_99_00=kmeans(proj_dt_99_00_wo_outlr,6, iter.max = 100,nstart = 3)
cl_99_00=clust_99_00$cluster
plot3d(proj_dt_99_00_wo_outlr,type = 's',col=cl_99_00,xlab = "PCA1",ylab = "PCA2",zlab =  "PCA3",radius = 0.3)










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


