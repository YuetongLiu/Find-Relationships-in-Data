##Multiple Linear Regression
###Model comparison with R^2 and adjusted R^2
###Partial Correlation.

#Read in the data set
dat<-read.csv("https://raw.githubusercontent.com/YuetongLiu/Find_Relationships_in_Data/master/Data/burnaby_condos.csv")

#Have a look at the data
head(dat)



#Only work with the numeric variables

#Similar as in the burnaby data set in the lectures, rescale same variables
dat$askprice <- dat$askprice/1000
dat$ffarea <- dat$ffarea/100
dat$mfee <- dat$mfee/10
dat$floor <- sqrt(dat$floor)

# ake region a categorical variable:
dat$region<-as.factor(dat$region)

#Have a look at the pairwise scatter plots (without MLS ids)
pairs(dat[,-1])

#Fit a simple linear regression with only ffarea,
sModel <- lm(askprice~ffarea, data=dat)
summary(sModel)

#Beds 
sModel2 <- lm(askprice~ffarea+beds, data=dat)
s2 <- summary(sModel2)
#Bath
sModel3 <- lm(askprice~ffarea+baths, data=dat)
s3 <- summary(sModel3)
#floor
sModel4 <- lm(askprice~ffarea+floor, data=dat)
s4 <- summary(sModel4)
#view
sModel5 <- lm(askprice~ffarea+view, data=dat)
s5 <- summary(sModel5)
#age
sModel6 <- lm(askprice~ffarea+age, data=dat)
s6 <- summary(sModel6)
#mfee
sModel7 <- lm(askprice~ffarea+mfee, data=dat)
s7 <- summary(sModel7)

adjR2 <- c(s2$adj.r.squared, s3$adj.r.squared, s4$adj.r.squared,
           s5$adj.r.squared, s6$adj.r.squared, s7$adj.r.squared)
names(adjR2) <- colnames(dat[-c(1:3,10)])
print(adjR2)


# Age has the largest adjusted R^2


##################Part 2: multicollinearity


#Fit a simple linear regression with only ffarea,
sModel <- lm(askprice~ffarea, data=dat)
summary(sModel)

#Fit the model with all variables
fModel <- lm(askprice~., data=dat[,-1])
# "." is an R syntax representing all the variables in your data frame except for 
# the response variable. 

summary(sModel)
summary(fModel)

#Here we see a small increase in the S.E., 

# check the Variance Inflation factor VIF
library(car)
vif(fModel)

#This number is larger than 1, but not very large.
#ffarea can still be included after all the other variables are included in the model.

# for comparison, check the variance inflation factor with the following added variable
cdat<-dat
n <- nrow(cdat)
set.seed(4321)
cdat$newvar <- 7*cdat$ffarea+rnorm(n,0,.02)

# check the Variance Inflation factor VIF
library(car)

newreg <- lm(askprice~., data=cdat[,-1])
vif(newreg)
summary(newreg)

#Fit the model with all variables  and an added interaction term:
IModel <- lm(askprice~askprice+   ffarea+     beds+       baths+      floor*view+       age+        mfee+       region, data=dat[,-1])
summary(IModel)



