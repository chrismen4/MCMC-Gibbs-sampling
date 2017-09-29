
# an example for dummy variables from the Michigan state university

# clean everything first 

ls()
rm(list=ls())
gc()

library(latex2exp)

mydata = read.table("fit-mixture-of-normal.txt", header = FALSE)  # read text file 

est=data.matrix(mydata) 
 
aa=as.matrix(mydata)

a1=dim(aa)

sub_sample=a1[1]

bb=aa[2:sub_sample,]

cc=apply(bb, 1,as.numeric)

cc=t(cc)


par(mar=c(3,3,3,3))

par(mfrow = c(3, 2))
plot(cc[,1],type = "l",col = "blue",xlab="",ylab="", cex.lab=1.25,cex.axis=2,main = TeX('Time series of $\\mu_1$'),cex.main=2)

plot(cc[,2],type = "l",col = "blue",xlab="",ylab="", cex.lab=1.25,cex.axis=2,main = TeX('Time series of $\\sigma_1$'),cex.main=2)
plot(cc[,3],type = "l",col = "blue",xlab="",ylab="", cex.lab=1.25,cex.axis=2,main = TeX('Time series of $\\rho$'),cex.main=2)
plot(cc[,4],type = "l",col = "blue",xlab="",ylab="", cex.lab=1.25,cex.axis=2,main = TeX('Time series of $\\mu_2$'),cex.main=2)
plot(cc[,5],type = "l",col = "blue",xlab="",ylab="", cex.lab=1.25,cex.axis=2,main = TeX('Time series of $\\sigma_2$'),cex.main=2)

bb=aa[500:sub_sample,]

cc=apply(bb, 1,as.numeric)

cc=t(cc)


apply(cc, 2, mean)
apply(cc, 2, sd)






