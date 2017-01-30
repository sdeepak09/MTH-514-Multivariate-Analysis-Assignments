data=read.csv("eco_dev_data.csv")
View(data)
sum(is.na(data)) ## as Sum is 0 so we can say that there is no missing values in the data 
data=data[,-1]  # Removed Country Names 
View(data)
mean_vec=as.numeric(colMeans(as.matrix(data)))
mean_mat=matrix(rep(mean_vec,nrow(data)),nrow = nrow(data),byrow = T)
View(mean_mat)
data_cen=data-mean_mat
View(data_cen)
data_cen_mat=as.matrix(data_cen)
cov_mat=t(data_cen_mat)%*%(data_cen_mat)
View(cov_mat)
cor_mat=cov_mat/120
View(cor_mat)
eig_val_vec=eigen(cor_mat)
eig_val=eig_val_vec$values
eig_vec=eig_val_vec$vectors
t(eig_vec[,1])%*%(eig_vec[,1])
data_new=as.matrix(data)%*%eig_vec
View(data_new)
install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(x=data_new[,1],y=data_new[,2],z=data_new[,3])
plot(data_new[,1],data_new[,2])
install.packages("rgl")
library(rgl)
plot3d(data_new[,1:3])
text3d(data_new[,1:3],texts=rownames(data))
data_new_1=data_new[-c(3,10,14,42,60,64,98,113),]
clust=kmeans(data_new_1,3, iter.max = 100,nstart = 3)
clust_nms=clust$cluster
plot3d(data_new_1[,1:3],type = 's',col=clust_nms,xlab = "PCA1",ylab = "PCA2",zlab =  "PCA3",radius = 0.15)
comptd_data=read.csv("eco_dev_data.csv")
country=as.vector(comptd_data[,1])
country_wo_outlr=country[-c(3,10,14,42,60,64,98,113)]
PCA1=data_new_1[,1]
dt=data.frame(country_wo_outlr,PCA1)
dt1=dt[order(dt[,2]),]
View(dt1)

## Variable Clustering



