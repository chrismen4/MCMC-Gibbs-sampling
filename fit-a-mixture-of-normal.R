

ls()
rm(list=ls())
gc()
library(invgamma)
library(LaplacesDemon)
# ---------------generate a mixture of two normal distribution------------
n =5000 # the sample size

b = seq(1:n)
z = seq(1:n)
mu1 = -0.75
mu2 = 0.75
sigma1 = 0.2
sigma2 = 0.6
rho= 0.25

for (i in 1:n)
{
  z1=rbern(1,rho)
  if (z1==1)
  {b[i]=rnorm(1,mu1,sigma1)}
  else
  { b[i]=rnorm(1,mu2, sigma2)}
}
par(mar=c(2,2,2,2))
hist(b, breaks=50,col="blue", xlab=" ",ylab=" ",yaxt="n",xaxt='n'  ,main=""  ) 
title(main="Histogram with density curve for a mixture of two normal distributions", 
      font = 2, cex.main = 2, col.main="blue")
#axis(2, col.axis="red", las=1,cex.axis=2)
par(new=TRUE)
# Kernel Density Plot
d <- density(b) # returns the density data 
plot(d, xlab="",ylab="", main = "",type="l",col="red", lwd=2 ,xaxt='n',yaxt='n') # plots the results
# axis(4,  col.axis="blue", las=2, cex.axis=2, tck=-.01)
# axis(1,  col.axis="blue", las=2, cex.axis=2, tck=-.01)

y=b
d0 <- data.frame(y)                ## example data
write.table(d0, "simulated-data-mix-of-normal.txt", row.names = FALSE,
            col.names = FALSE)
plot(y)


# -----------------------  now we start the estimation process of MCMC
d1 <- data.frame(rho, mu1, mu2, sigma1, sigma2)                
write.table(d1, "fit-mixture-of-normal.txt", row.names = FALSE)

m=20000 # the length of the MCMC samples
z = seq(1:n)
for (i in 1:m)
{
  print(i)
  # ---------  we sample the latent variables first 
  for (j in 1:n)
  {
    temp1 = rho / sigma1 * exp(-(y[j] - mu1)^2 / 2 / sigma1 ^ 2)
    temp2 = (1 - rho) / sigma2 * exp(-(y[j] - mu2) ^ 2 / 2 / sigma2 ^ 2)
    temp = temp1 / (temp1 + temp2)
    z[j] =  rbinom(1, 1, temp)
  }
  
  # we sample rho
  t1=sum(z==1)
  t2=sum(z==0)
  rho=rbeta(1, t1+1, t2+1)
  
  # --------------------sample mu1
  a1= sum(z==1)/sigma1^2
  b1=0
  
  for (j in 1:n)
  {
    if (z[j] == 1)
    {
      b1 = b1 +  y[j] /sigma1 ^ 2
    }
  }
  mu1 = rnorm(1, b1/ a1, sqrt(1 / a1))
  
  # ----------------------sample mu2
  a1= sum(z==0)/sigma2^2
  b1=0
  for (j in 1:n)
  {
    if (z[j] == 0)
    {
      b1 = b1 +  y[j] / sigma2 ^ 2
    }
  }
  mu2 = rnorm(1, b1/ a1, sqrt(1 / a1))
  
  # sample sigma1^2
  a1= sum(z==1)
  b1=0
  for (j in 1:n)
  {
    if (z[j] == 1)
    {
      b1 = b1 +  (y[j] -mu1)^2 / 2
    }
  }
  sigma1= sqrt(rinvgamma(1, a1/2, b1))
  
  # sample sigma2^2
  a1= sum(z==0)
  b1=0
  for (j in 1:n)
  {
    if (z[j] == 0)
    {
      b1 = b1 +  (y[j] - mu2)^2 / 2
    }
  }
  sigma2= sqrt(rinvgamma(1, a1/2, b1))
  
  d1 <- data.frame(rho, mu1, mu2, sigma1, sigma2)
  write.table(d1, "fit-mixture-of-normal.txt", row.names = FALSE,
              col.names = FALSE, append = TRUE)
}



 
 
 
 
 