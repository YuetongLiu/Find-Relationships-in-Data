# Simulation to show sampling distribution of beta1hat, sigmahat, t-ratios etc
# Sampling distribution of an estimator such as beta1hat, refers to the
# distribution of the estimator over different samples, considered as
# different realizations from the model.
# Suppose fixed values beta0, beta1 as population parameters 
# Data are (x_1,y_1),...(x_i,y_i), ... (x_n,y_n) 
#  where x is the explanatory variable; 
#  and y is the response variable.
# y_i are realizations of random variables Y_i where
#    Y_i = beta0 + beta1*x_i + epsilon_i
# random deviations from the line epsilon_i have N(0,sigma^2) distribution,  i=1,...,n
#

set.seed(1223)  # set seed to get same thing if rerun again
n <- 9  # small sample size
x1 <- (1:n)-(n+1)/2   # fixed x values at 1 to n; centred by subtracting average

# set (true) population parameters
b0 <- 1
b1 <- 0.7
sigma <- 0.5

nsim <- 100   # number of replications
#nsim <- 1000   # uncomment this line to do more replications
b1hat <- rep(0,nsim) # pre-allocate space to store estimates/statistics from replications
b0hat <- rep(0,nsim)
sigmahat <- rep(0,nsim)
t1ratio <- rep(0,nsim)

# simulation/replication loop
png("Simulation.png", width=1080, height=960)
par(mfrow=c(4,3))  # set-up for 4x3 subplots

for(i in 1:nsim)
{ err <- rnorm(n,0,sigma)     # random from normal(mean=0,SD=sigma)
  y <- b0+b1*x1+err           # generate y's with deviations in 'err'
  datareg <- as.data.frame(cbind(y, x1))
  reg <- lm(y~x1,data=datareg)  # regression 
  summ <- summary(reg)
  coefftab <- summ$coeff           # get betas and their SEs
  b0hat[i] <- coefftab[1,1]       # coefficent beta0hat
  b1hat[i] <- coefftab[2,1]
  se1hat <- coefftab[2,2]        # SE of beta1hat
  sigmahat[i] <- summ$sigma        # residual SE or residual SD
  t1ratio[i] <- (b1hat[i]-b1)/se1hat  # t-ratio for beta1hat
  if(i <= 12)                     # plot the first few cases 
  { plot(x1, y, xlim=c(x1[1],x1[n]), ylim=c(b0+b1*x1[1]-3,b0+b1*x1[n]+3))
    abline(b0hat[i], b1hat[i], col=1)
    title(paste("Simulation ", i))
  }
}
dev.off() #This line is finishing the png file.


# Under the assumptions of the model ( epsilon_i ~ N(0,sigma^2) # independently)
# then

# (a) sampling distribution of b1hat is normal with mean b1
#     normal quantile plot of b1hat over different replications
#     shows points very close to a straight line
par(mfrow=c(2,2))
qqnorm(b1hat,main="b1hat qqnorm")
qqline(b1hat)

# (b) sampling distribution of t-ratio is not normal because denominator 
#   involves a random sigmahat. The sampling distribution is Student t with
#   degrees of freedom n-k, where  k is the number of estimated betas.
# t-density has heavier tails than normal density, so normal quantile
#   plots shows points off the diagonal at the two ends.
qqnorm(t1ratio,main="t1ratio qqnorm") 
qqline(t1ratio)

# (c) sampling distribution of sigmahat^2 is right-skewed; when scaled
#   appropriately, it has a distribution called chi-squared.
hist(sigmahat^2,main="scaled chi2 dist.")

# (d) b1hat has zero theoretical correlation with sigmahat
#     This property is involved in the mathematical derivation of the
#     Student t distribution for the t-ratio.
#  the actual correlation of b1hat will be closer to 0 with larger nsim
plot(b1hat,sigmahat) 
title(paste("cor=",round(cor(b1hat,sigmahat),2),", nsim=",nsim) )

