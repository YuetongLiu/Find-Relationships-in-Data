### Multiple Linear Regression
### variable selection using regsubsets ("leaps" package)
### and cross-validation using "train" function from "caret" package
### PART 1: Exhaustive selection.
### PART 2: Forward selection.
### PART 3: Backward selection. 
### PART 4: K-Cross-Validation.

# A small function to calculate partial correlations:
pcor <- function(s)
{ i1 <- c(1, 2)
  i2 <- 3:nrow(s); 
  s11 <- s[i1,i1]; s12 <- s[i1,i2]; s21 <- s[i2,i1]; s22  <- s[i2,i2];
  condcov <- s11 - s12 %*% solve(s22) %*% s21
  return(condcov[1,2]/sqrt(condcov[1,1] * condcov[2,2]))
}

#Read in the data set
dat<-read.csv("https://raw.githubusercontent.com/YuetongLiu/Find_Relationships_in_Data/master/Data/burnaby_condos.csv")

# Have a look at the data:
head(dat)
tail(dat)
summary(dat)

dat <- dat[,-c(1, 10)]

# Scale the numbers to make them a bit more manageable:
dat$askprice <- dat$askprice/1000
dat$ffarea <- dat$ffarea/100
dat$mfee <- dat$mfee/10

# Transform floor to sqrt(floor), 
# based on our oown knowledge of how "floor" might impact the price: 
dat$floor <- sqrt(dat$floor)

# Install the "leaps" package
install.packages("leaps")
library(leaps)

###############################################################
### PART 1:  Exhaustive selection.
# regsubsets will perform an exhaustive search of the best model 
# with different number of variables, i.e.
# It will find the best model with one variable, best model with two variables, etc.

s1 <- regsubsets(askprice~., data=dat, method="exhaustive")
ss1 <- summary(s1)
ss1

#This table can be accessed directly via 
ss1$which
#where "TRUE" is in the place of "*"

#Get the adjusted R^2 of each model
ss1$adjr2
which.max(ss1$adjr2)
#Best model is with 4 variables according to adj-R^2

#Get the cp of each model
ss1$cp
which.min(ss1$cp)
#Best model is with 4 variables according to Cp

###############################################################
### PART 2: Forward selection.
###The procedure starts from no explanatory variables, 
###add one variable at each step.
s2 <- regsubsets(askprice~., data=dat, method="forward")
ss2 <- summary(s2)
ss2

###Verify the first a few steps in forward selection.
#Step 1: find the variable with highest corrleation with y.
cormat <- cor(dat)
cormat
cormat[1,-1]

# baths has the highest absolute correlation with askprice.

#Step 2, find the highest partial correlation conditioning on baths.
pcorStep2 <-rep(NA, 6)
# Loop over all the variables except baths (indexed by number 4).
for (j in c(2,3,5,6,7,8))
{ cat(colnames(dat)[j], "\t")
  index <- c(1,j,4)
  tpcor <- pcor(cormat[index,index])
  cat(tpcor,"\n")
  pcorStep2[j-1] <- tpcor
}

# floor has highest absolute partial correlation with askprice.

#Step 3, continuing to find the largest partial correlation conditioning on baths and floor
pcorStep2 <-rep(NA, 5)
# Loop over all the variables.
for (j in c(2,3,6,7,8))
{ cat(colnames(dat)[j], "\t")
  index <- c(1,j,4,5)
  tpcor <- pcor(cormat[index,index])
  cat(tpcor,"\n")
  pcorStep2[j-1] <- tpcor
}

# age has highest absolute partial correlation with askprice.

# so the first three variables are: baths, floor, and age
#  This is what regsubsets cam up with:
ss2

###############################################################
### PART 3: Backward selection. Starts from a model with all the variables
###delete one variable at each step.
s3 <- regsubsets(askprice~., data=dat, method="backward")
ss3 <- summary(s3)
ss3


###############################################################
### PART 4: K-Cross-Validation: 
### Find the "best" model for predictions using Cross-Validation:

library(caret)

# set random seed so that we can replicate our findings:
set.seed(123)

# We will calculate 5-fold CV-RMSE for 4 potential models:


train(log(askprice)~baths+floor+age, data=dat, method="lm", 
trControl=trainControl(method="cv",number=5))

train(log(askprice)~baths+floor+age+view+ffarea, data=dat, method="lm", 
trControl=trainControl(method="cv",number=5))

train(log(askprice)~baths+floor+age+view+ffarea+mfee, data=dat, method="lm", 
trControl=trainControl(method="cv",number=5))

train(log(askprice)~baths+floor+age+view+ffarea+mfee+beds, data=dat, method="lm", 
trControl=trainControl(method="cv",number=5))

train(log(askprice)~ baths+age+ffarea+mfee+floor+floor:view, data=dat, method="lm", 
trControl=trainControl(method="cv",number=5))



