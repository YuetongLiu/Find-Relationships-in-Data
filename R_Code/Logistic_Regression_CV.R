####Logistic regression
####Model selection based on AIC
####Cross validation.

#Working with low birth weight data
#Read in the data
wt <- read.table("https://raw.githubusercontent.com/YuetongLiu/Find_Relationships_in_Data/master/Data/bwt.txt", header=T)
wt$race <- as.factor(wt$race)
wt$ftv <- as.factor(wt$ftv)


#The model with all varaibles
fit1 <- glm(low~., data=wt, family="binomial")
summary(fit1)

#Another way to calculate the AIC
AIC(fit1)

#A model with only the significant variables in fit1
fit2 <- glm(low~lwt+race+smoke+ht+ptd, data=wt, family="binomial")
summary(fit2)

AIC(fit2)
#smaller than that of fit1.

#How to perform automatic model selection for logistic regression.
sel1 <- step(fit1, direction="backward")

#The resulting model is low ~ age + lwt + race + smoke + ptd + ht + ui

summary(sel1)
#.....
#   smoke        0.866582   0.404469   2.143  0.03215 * 
#   ptd          1.128857   0.450388   2.506  0.01220 * 
#   ht           1.866895   0.707373   2.639  0.00831 **
#   ui           0.750649   0.458815   1.636  0.10183   
# ....

# Notice here ui is not significant at the 5% level, 
# but deleting it will not result in a smaller AIC. 


##Two-fold cross-validation.
##As in previous examples, we can split the data into two sets and
##test the predictive performance of the logistic models.
set.seed(1)
ind <- sample(1:189, 90)  # random split!
wt.sub1 <- wt[-ind, ]
wt.sub2 <- wt[ind, ]

fit.sub1 <- glm(low~lwt+race+smoke+ht+ptd, data=wt.sub1, family="binomial")
pred.prob1 <- predict(fit.sub1, newdata=wt.sub2, type="response")
pt1 <- table(wt.sub2$low, as.numeric(pred.prob1>0.5))
pt1

#Reverse the training and hold-out set.
fit.sub2 <- glm(low~lwt+race+smoke+ht+ptd, data=wt.sub2, family="binomial")
pred.prob2 <- predict(fit.sub2, newdata=wt.sub1, type="response")
pt2 <- table(wt.sub1$low, as.numeric(pred.prob2>0.5))
pt2

##Total misclassification rate
(pt1[1,2] + pt1[2, 1] + pt2[1,2] + pt2[2, 1])/189
#0.3227513
#Modify the above code to calculate the misclassification rate of the full model. 


##Effect of changing the cut-off value in the prediction
#What will happen if we change the cut-off probability to 0.3?
pt11.3 <- table(wt.sub2$low, as.numeric(pred.prob1>0.3))
pt11.3
#Compare to pt1
pt1
#More misclassifications in the 0 (no low birth weight) cases 
#while less misclassifications in the 1 cases. 
(pt11.3[2,1] + pt11.3[1,2])/nrow(wt.sub2) 

#If we increase the cut-off point to 0.7
pt11.7 <- table(wt.sub2$low, as.numeric(pred.prob1>0.7))
pt11.7
(pt11.7[2,1] + pt11.7[1,2])/nrow(wt.sub2) 

#However, we do not use the misclassification rate to choose the cut-off point. 
#The cut-off point can be chosen based on the losses (costs) of misclassifications (at each case),
# which will not be covered in this course.

# the SEs can be calculated by

s1 <- summary(fit1)
sqrt(diag(s1$cov.unscaled))

summary(fit1)


## Null deviance:
fit1$null.deviance

## First method:
## Recall that a null model is a model with no explanatory variable.

## Therefore, the regression model only has a constant term.

fit_null<- glm(low~1, data=wt, family="binomial")
## The null deviance, or the deviance of the null model is
fit_null$deviance

## Second method:
## Use Equation 6.40 from coursepack
y <- wt$low
-2 * (sum(y*log(mean(y))  +(1-y)*log(1-mean(y))))
