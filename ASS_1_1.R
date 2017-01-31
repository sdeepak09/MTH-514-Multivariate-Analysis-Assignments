data=read.csv("eco_dev_data.csv")
data=data[,-1]  # Removed Country Names 
data_cen=scale(data)
data_cen_mat=as.matrix(data_cen)
cov_mat=t(data_cen_mat)%*%(data_cen_mat)
#View(cor_mat)
cor_mat=cov_mat/120
eig_val_vec=eigen(cor_mat)
eig_val=eig_val_vec$values
eig_vec=eig_val_vec$vectors
data_new=as.matrix(data_cen_mat)%*%eig_vec[,1:3]
library(rgl)
plot3d(data_new,type = 's',xlab = "PC1",ylab = "PC2",zlab =  "PC3",radius = 0.01)
text3d(data_new,texts=rownames(data),col="red")


data_1=data[-c(3,5,8,10,14,15,25,42,57,60,63,64,72,93,98,106,113),]
data_cen_1=scale(data_1)
data_cen_mat_1=as.matrix(data_cen_1)
cov_mat_1=t(data_cen_mat_1)%*%(data_cen_mat_1)
View(cov_mat_1)
cor_mat_1=cov_mat_1/103
eig_val_vec_1=eigen(cor_mat_1)
eig_val_1=eig_val_vec_1$values
eig_vec_1=eig_val_vec_1$vectors
data_new_1=as.matrix(data_cen_mat_1)%*%eig_vec_1[,1:3]
View(data_new_1)
plot3d(data_new_1,type = 's',xlab = "PC1",ylab = "PC2",zlab =  "PC3",radius = 0.01)
text3d(data_new_1,texts=as.factor(c(1:103)),col="red")


## Clustering of the countries
clust=kmeans(data_new_1,5, iter.max = 100,nstart = 3)
cl=clust$cluster
plot3d(data_new_1,type = 's',col=cl,xlab = "PCA1",ylab = "PCA2",zlab =  "PCA3",radius = 0.3)

# Ranking of countries
comptd_data=read.csv("eco_dev_data.csv")
country=as.vector(comptd_data[,1])
country_wo_outlr=country[-c(3,5,8,10,14,15,25,42,57,60,63,64,72,93,98,106,113)]
PCA1=data_new_1[,1]
dt=data.frame(country_wo_outlr,PCA1)
dt1=dt[order(-dt[,2]),]
View(dt1)   ## Good 

## Variable Clustering
first_3_PCs=eig_vec_1[,1:3]
first_3_lambda=eig_val_1[1:3]
sqrt_first_3_lambda=sqrt(first_3_lambda)
mat=matrix(rep(sqrt_first_3_lambda,nrow(first_3_PCs)),nrow = nrow(first_3_PCs),byrow = F)
final_mat=first_3_PCs*mat
View(final_mat)
cl=kmeans(final_mat,4, iter.max = 100,nstart = 3)
cl_nms=cl$cluster
plot3d(final_mat,type = 's',col=cl_nms,xlab = "PCA1",ylab = "PCA2",zlab =  "PCA3",radius = 0.1)

















