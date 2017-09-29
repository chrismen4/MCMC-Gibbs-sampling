


# clean everything first 


library(latex2exp)



ls()
rm(list=ls())
gc()


mydata = read.table("fit_normal-time-series.txt",  header = FALSE)  # read text file 

est=data.matrix(mydata) 
 
aa=as.matrix(mydata)

a1=dim(aa)

sub_sample=a1[1]

bb=aa[2:sub_sample,]

cc=apply(bb, 1,as.numeric)

cc=t(cc)

par(mar=c(3,3,3,3))
par(mfrow = c(2, 1))
plot(cc[,1],type = "l",col = "blue",xlab="",ylab="", cex.lab=1.25,cex.axis=2,main = TeX('Time series of $\\mu$'),cex.main=2)
plot(cc[,2],type = "l",col = "blue",xlab="",ylab="", cex.lab=1.25,cex.axis=2,main = TeX('Time series of $\\sigma$'),cex.main=2)

bb1=aa[500:sub_sample,]

apply(bb1, 2, mean)
apply(bb1, 2, sd)



