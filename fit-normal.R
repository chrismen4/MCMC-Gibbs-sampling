
ls()
rm(list=ls())
gc()
library(invgamma)
n =4000 # the sample size
beta1 = 2
beta2 = 3
mu = -0.75
sigma = 0.2
y=rnorm(n,mu, sigma)
d0=data.frame(y)
write.table(d0, "simulated-data-normal.txt", row.names = FALSE,
            col.names = FALSE)

d1 <- data.frame(mu,  sigma)                
write.table(d1, "fit_normal-time-series.txt", row.names = FALSE)


mu_star=0.001
sigma_star=10

alpha_sigma=0.0001
beta_sigma=0.0001

m=20000 # the length of the MCMC samples
for (i in 1:m)
{
  print(i)
  # --------------  sample mu
  B = mu_star/sigma_star^2 + sum(y)/sigma^2
  A = 1/sigma_star^2  +n/sigma^2
  mu = rnorm(1, B/A, sqrt(1 / A))
  
  # sample sigma^2
  a = alpha_sigma +  n/2
  b = beta_sigma + sum( (y-mu)^2)/2
  
  sigma= sqrt(rinvgamma(1, a, b))
  d1 <- data.frame(mu, sigma)
  write.table(d1, "fit_normal-time-series.txt", row.names = FALSE,
              col.names = FALSE, append = TRUE)
}


 
 
 
 
 
 