# Multiple Linear Regression with
# categorical explanatory variable with more than 2 categories, 
# binary dummy variables and variable transformations.
# Also model diagnostics with residual plots.

# Read in the data set
mov <- read.table("https://raw.githubusercontent.com/YuetongLiu/Find_Relationships_in_Data/master/Data/moviegross.txt", header=T, skip=2)
names(mov)
# Variables are: year, movie, studio, openweekendgross, gross,  ST
# ST is a shorter version of studio. 
# open weekend gross and gross (final gross) are in million U.S. dollars.

# The goal is to relate the opening weekend gross with the "final" gross
#   for the US market, another explanatory variable might be the
#   studio that produced the movie;
mov$lngross <- log(mov$gross)
mov$lnopen <- log(mov$openweekendgross)
attach(mov)

print(cor(openweekendgross,gross))  # 0.827
print(cor(lnopen,lngross))          # 0.867

# Regression on the original scale
par(mfrow=c(2,4), mar=c(4, 4, 1, 1))
plot(openweekendgross, gross, type="n")
text(openweekendgross, gross, label=ST) 
fit <- lm(gross ~ openweekendgross + ST)
print(summary(fit))

residSE <- sqrt(sum(fit$resid^2)/fit$df.resid)
plot(openweekendgross, fit$resid)
plot(fit$fitted, fit$resid)
abline(h=2*residSE)
abline(h=-2*residSE)

qqnorm(fit$residuals)
qqline(fit$residuals, col="red")

##Fit the model to the log transformed data.
plot(lnopen, lngross, type="n")
text(lnopen, lngross, label=ST)  
fit2 <- lm(lngross ~ lnopen + ST)
print(summary(fit2))

#The log transformed model has greater R-squared

residSE2 <- sqrt(sum(fit2$resid^2)/fit2$df.resid)
plot(lnopen, fit2$resid)
plot(fit2$fitted, fit2$resid, ylim=c(-1, 1))
abline(h=2*residSE2)
abline(h=-2*residSE2)

qqnorm(fit2$residuals)
qqline(fit2$residuals, col="red")

#Compare the first column of the plots, a linear model is better in the log transformed data.
#In the second column, the variance is more homogeneous in the transformed data. 
#For the third column, we have less outliers in the residual plots
#For the fourth column, the deviation from the qqline is less severe in the lower tail. 

# Dummy variables
print(table(studio))
#studio

print(table(ST))

class(ST)
#ST is a "factor" in the data frame.

levels(ST)
# disney is the "baseline" studio in the regression below,

ST <- relevel(ST, 3)
summary(lm(lngross ~ lnopen + ST))

M <- model.matrix(fit)
#Obtain the design matrix of our regression model 

#Check the design matrix of the dummy variable.
head(mov, 10)
head(M, 10)
#Or you can compare the whole matrix.
#Or
which(as.numeric(M[,3])!=0)
which(ST=="f")

