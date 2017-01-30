## Assignment 2 Answer of question 2
data=read.csv("q2_data.csv")
View(data)
data=data[,-21]
bank_vec=data[,1]
dt_new=data[,-1]
View(dt_new)
sum(is.na(dt_new)) ## as Sum is 0 so we can say that there is no missing values in the data 
mn_vec=as.numeric(colMeans(as.matrix(dt_new)))
mn_mat=matrix(rep(mn_vec,nrow(dt_new)),nrow = nrow(dt_new),byrow = T)
dt_cen=dt_new-mn_mat
dt_cen_mat=as.matrix(dt_cen)
cov_mat=t(dt_cen_mat)%*%(dt_cen_mat)





