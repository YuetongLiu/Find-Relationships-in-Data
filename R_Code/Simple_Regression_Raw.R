# Fit a Simple Regression Model

# 1. Create data matrix and plot the data
dat <- read.table("https://raw.githubusercontent.com/YuetongLiu/Find_Relationships_in_Data/master/Data/Age_vs_Money_data.csv",  header = TRUE, sep=",")

# have a look at the data
dat
# print out the summary of this data set
summary(dat)
# Age is the age in two categories "old" (=0) and "young" (=1)
# Age_years is the age in years
# Money is in dollars 

# plot data
plot(Money ~ Age_years, data = dat)

# make the plot a bit nicer with axis limits and labels:
plot(Money ~ Age_years, data = dat, xlab="Age (years)", ylab="Money ($)", xlim=c(0,100), ylim=c(0,100))


# calculate the variance, covariance, correlation, etc.
# variance of Money
var(dat$Money)
# or
var(dat[, 3])

# correlation of Age_years and Money
cor(dat$Age_years, dat$Money)

# covariance matrix of Age_years and Money
cov(dat[, -1])

# 2. Simple Linear Regression:

# We will use Money as response variable (y) and Age_years as the explanatory variable (x)
x <- dat$Age_years
y <- dat$Money

# Calculate mean and standard deviation of x and y.

mean(x)
sd(x)
mean(y)
mean(y)

# Calculate beta0 hat (estimate of intercept term) and
# beta1 hat (estimate of slope coefficient)

# fit the regression line and summarize the linear 
# regression result using R function lm
reg <- lm(y~x)
# summary of linear regression model displays 
# coefficient estimates, with se, t statistics and p-values
summary(reg)

# to check what sorts of output components are available
names(reg)

# extract estimated coefficients, beta0 hat and beta1 hat
reg$coef
b0 <- reg$coef[1]
b1 <- reg$coef[2]
c(b0,b1)
# compare with formula for least squares slope and intercept
print(cor(x, y) * sd(y) / sd(x))
print(mean(y) - b1 * mean(x))

# add one straight line into the current plot with the intercept b0 and slope b1
plot(Money ~ Age_years, data = dat, xlab="Age (years)", ylab="Money ($)", xlim=c(0,100), ylim=c(0,100))
abline(b0, b1, col = "red")

# 3. Calculate SS(Res) and sigma^2
n <- length(y)
res <- reg$res
ss.res <- sum(res ^ 2)
sigma2 <- ss.res / (n - 2)
sigma2
# compare with "Residual Standard Error" in summary(reg) output
sqrt(sigma2)
summary(reg)

# 4. Compute the standard errors of beta0 hat and beta1 hat
# Computes basic statistics, including se, t- and p-values for the regression
# coefficients
coefficients(summary(reg))
# some parts of this diag is not required in this course.

# extract standard errors for b0 and b1
b0.se <-  coefficients(summary(reg))[1,2]
b1.se <-  coefficients(summary(reg))[2,2]
print(b1.se)

# compare formula for SE(beta1hat) in coursepack
print(sqrt(sigma2) / (sqrt(n - 1) * sd(x)))

# 5. Test H0: beta1=0
t.b1 <- abs(b1 / b1.se)
t.b1
p.value <- 2 * (1 - pt(t.b1, n - 2))
p.value

# compare with output using summary:
summary(reg)

# 6. calculate 95% confidence intervals for b1
# compute 0.975 quantile of the t distribution with degrees of freedom n-2
tcv <- qt(0.975, n - 2)
# calculate 95% confidence intervals for b1
b1.ci <- c(b1 - tcv * b1.se, b1 + tcv * b1.se)
b1.ci

# compare with output using confint
confint(reg)

# 7. Prediction at a new observation x=50 and calculate the confidence interval 
# and the prediction interval
newx <- 50
newy <- b0 + b1 * newx


# confidence interval for subpopulation mean
xss <- (n - 1) * var(x)
newy.se <-  sqrt(sigma2) * sqrt(1 / n + (newx - mean(x)) ^ 2 / xss)
newy.ci <- c(newy - tcv * newy.se, newy + tcv * newy.se)
newy.ci

# prediction interval of newy
newy.pe <- sqrt(sigma2) * sqrt(1 + 1 / n + (newx - mean(x)) ^ 2 / xss)
newy.pi <- c(newy - tcv * newy.pe, newy + tcv * newy.pe)
newy.pi

# 9. Calculate the regression coefficients by yourself on a subset of the data
# For example, we are now only interested only in adults (age>18)
# get those x and call them x1 using "x[x...]" for "x such that x..."
x1 <- x[x > 18]
# get the corresponding y1
y1 <- y[x > 18]
n1 <- length(x1)


# calculate the summary statistics as in Page 12 of the Course pack
mx1 <- mean(x1)
my1 <- mean(y1)
sdx1 <- sd(x1)
sdy1 <- sd(y1)
rxy1 <- cor(x1, y1)
# Calculate the slope
nb1 <- rxy1 * sdy1 / sdx1
print(nb1)
# Carry on to calculate the intercept (b0) for this subset data

# Verify your results using the lsfit function.
lm(y1~x1)$coef







