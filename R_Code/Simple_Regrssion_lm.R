## Fitting simple linear regression model using the statement "lm" in R.
## Checking assumptions of linear regression.

#Read-in the data as we did in Lab 1 and 2.
dat <- read.table("https://raw.githubusercontent.com/YuetongLiu/Find_Relationships_in_Data/master/Data/Age_vs_Money_data.csv",  header = TRUE, sep=",")

# A review of some R basics
# To view the first 5 rows of the data file
dat[1:5,]
#Or 
head(dat)

# To delete (if you want to do so) the second and third columns of the data:
tdat <- dat[,-c(2,3)]
tdat

# Or get the first, fifth and ninth rows from the data:
tdat2 <- dat[c(1, 5, 9), ]
tdat2

# To fit a linear regression line
reg <- lm(Money ~ Age_years , data=dat)
# Or 
#reg2 = lm(dat$Money ~ dat$Age_years)

# lm is one of many different ways of fitting a "linear model" (lm)
lsfit(dat$Age_years, dat$Money)

# To attach the linear regression line to the plot
plot(Money ~ Age_years , data=dat)
abline(reg, col="red", lty=2, lwd=2)

# To see the summary of the result
summary(reg)

# To see what's included in the object reg
ls(reg)
# Or
names(reg)

# To extract the residuals (or errors) of the fit
reg$res
# or you may want to type the full name
reg$residuals

# To extract the fitted values
reg$fitted.values

# Confirm that the residuals are simply...
dat$Money-reg$fitted.values

# To extract the estimated coefficients
summ <-summary(reg)
betatable <- summ$coefficients

#	or
betatable[,1]

# To extract the standard errors of the estimated coefficients
betatable[,2]

# To extract the t-ratios or test statistics for the regression coefficients
betatable[,3]

# To extract the p-values of the t-tests
betatable[,4]

# You can also check what's included in the summary of reg
ls(summ)


# To construct 95% confidence intervals for intercept and slope. 95% is the default.
confint(reg)
# To construct 99% confidence intervals for intercept and slope.
confint(reg, level = 0.99)

# 95% confidence interval for the mean response when Age equals 40 or 50
new <- data.frame(Age_years = c(40, 50)) 

# a prediction of the average money for an individual with Age equal 40 and 50
# with a confidence interval around this estimated average: 
newpred <- predict(reg, new,  se.fit = TRUE, interval="confidence", level=0.95)
newpred

# Alternatively one might be interested in a prediciton interval
# Here is a 80% prediction interval of money for different values Age.
new <- data.frame(Age_years = c(40, 50)) 
newpred <- predict(reg, new,  se.fit = TRUE, interval="prediction", level=0.80)
newpred

## Bad example of not inputting a data.frame
predict(reg, new=c(0, 2),  se.fit = TRUE, interval="prediction", level=0.99)
lm(Y~X, data=cbind(Y=1:10, X=4:13))

#But the following is good
lm(Y~X, data=data.frame(cbind(Y=1:10, X=4:13)))

example.df<-data.frame("words"=c("text1","word2","another"), "T_or_F"=c(TRUE,FALSE,FALSE), "variable2"=c(3.23,-0.2,33))
example.df
class(example.df)
class(example.df$logicals)
class(example.df$variable2)


# SSE is the "sum of squared errors"
# SST is the "sum of squared total"  (sum of squared errors on would have without the model) 

SSE <- sum(reg$res^2) 
SST <- sum((dat$Money - mean(dat$Money))^2)

# calculate the Rsquared:
R_squared<-1-SSE/SST
R_squared

# Compare this with the output from lm:
summary(reg)

######## Bonus material ########

## Generate two sets of 100 random numbers
## Fit a linear regression model
## And check to see if the slope is statistically significant (not equal to 0)

random_numbers1<-rnorm(100)
random_numbers2<-rnorm(100)
random_numbers1
random_numbers2
random_mod<-lm(random_numbers2~random_numbers1)
random_summary <-summary(random_mod)
random_summary
beta1_pval<-coef(random_summary)[2,4]
beta1_pval





