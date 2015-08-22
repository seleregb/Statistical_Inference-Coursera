# Setting the seed
set.seed(133233)
lambda <- 0.2 # lambda value
nobserv <- 1000 # number of observations
sample_size <- 40 # sample size


exp_sample_means <- c() # empty vector to store results

# looping through sample
for(i in 1:nobserv) {
  exp_sample_means <- c(exp_sample_means, mean(rexp(sample_size, lambda)))
}

# Calculating mean of the sample
mean(exp_sample_means)

round(mean(exp_sample_means), 2)

actual_mean <- mean(exp_sample_means)

theor_mean <- abs(-1/lambda)

# histogram to show the comparison of the theroetical and actual mean
par(mfrow=c(1,2))
hist(exp_sample_means,probability=T,main=paste('Theoretical Mean of ',format(theor_mean,nsmall=2)),ylim=c(0,0.55),col='blue',xlab='Means of Simulation Set')
abline(v=theor_mean,col='red',lwd=5)
hist(exp_sample_means,probability=T,main=paste('Actual Mean of ',format(actual_mean,nsmall=2)),ylim=c(0,0.55),col='blue',xlab='Means of Simulation Set')
abline(v=actual_mean,col='green',lwd=5)

actual_var <- var(exp_sample_means) 

actual_var

# Comparison of the actual and theoretical variance from the distribution
par(mfrow=c(1,1))
hist(scale(exp_sample_means),probability=T,main=" Comparison of the actual distribution\n and the normal distribution",ylim=c(0,0.5),xlab='', col = 'grey')
curve(dnorm(x,0,1),-3,3, col='red',add=TRUE, lwd = 3) # normal distribution
lines(density(scale(exp_sample_means)),col='blue', lwd = 3) # actual distribution 
legend(2,0.4,c('Normal','Actual'),cex=0.8,col=c('red','blue'),lty=1)