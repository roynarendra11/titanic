
# Logistic Regression -----------------------------------------------------

library(caret)
library(psych)
set.seed(123)
train %>% select_if(is.numeric) %>% cor()

valid <- train[createDataPartition(train$Survived,times = 1,p = 0.3)$`Resample1`,]
train <- train[-createDataPartition(train$Survived,times = 1,p = 0.3)$`Resample1`,]

contrasts(train$Sex)

summary(train)

train$Sex <- relevel(train$Sex,ref='male')
train$mar_sts <- relevel(train$mar_sts, ref='Others')
train$Pclass <- relevel(train$Pclass, ref='3')

model.glm <- step(glm(Survived ~ Sex + Pclass+Age+SibSp+Parch+Fare+Sex*Pclass,data = train, family=binomial(link='logit')))

summary(model.glm)

# > summary(model.glm)
# 
# Call:
#   glm(formula = Survived ~ Sex + Pclass + Age + SibSp + Sex:Pclass, 
#       family = binomial(link = "logit"), data = train)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -3.2463  -0.6597  -0.5006   0.4059   2.4635  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -0.29316    0.33196  -0.883 0.377172    
# Sexfemale          1.63776    0.29016   5.644 1.66e-08 ***
#   Pclass1            1.85175    0.33488   5.530 3.21e-08 ***
#   Pclass2            0.60152    0.34065   1.766 0.077426 .  
# Age               -0.05312    0.01049  -5.063 4.13e-07 ***
#   SibSp             -0.39265    0.12354  -3.178 0.001481 ** 
#   Sexfemale:Pclass1  2.56661    0.81887   3.134 0.001722 ** 
#   Sexfemale:Pclass2  2.16219    0.61379   3.523 0.000427 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 829.6  on 622  degrees of freedom
# Residual deviance: 544.1  on 615  degrees of freedom
# AIC: 560.1
# 
# Number of Fisher Scoring iterations: 6


train$predict <- predict(model.glm,type ="response" )
summary(train$predict)

modelMetrics <- function(actual,predicted,thr){
  t <- table(actual, predicted>thr)
  precision <-  t[2,2]/sum(t[,2])
  recall <- t[2,2]/sum(t[2,])
  f_score <- 2*precision*recall/(precision+recall)
  
  print(t)
  cat("Precision:",precision,'\n')
  cat("Recall:",recall,'\n')
  cat("F-Score:",f_score,'\n')
}

modelMetrics(train$Survived,train$predict,0.5)
modelMetrics(train$Survived,train$predict,0.6)

# > modelMetrics(train$Survived,train$predict,0.5)
# 
# actual FALSE TRUE
# 0   352   32
# 1    85  154
# Precision: 0.827957 
# Recall: 0.6443515 
# F-Score: 0.7247059 

# > modelMetrics(train$Survived,train$predict,0.6)
# actual FALSE TRUE
# 0   372   12
# 1   110  129
# Precision: 0.9148936 
# Recall: 0.539749 
# F-Score: 0.6789474


# Checking on Validation set ----------------------------------------------

valid$pred <- predict(model.glm,newdata = valid,type = "response")
summary(valid$pred)


modelMetrics(valid$Survived,valid$pred,0.5)
modelMetrics(valid$Survived,valid$pred,0.6)
# 
# > modelMetrics(valid$Survived,valid$pred,0.5)
# 
# actual FALSE TRUE
# 0   151   14
# 1    45   58
# Precision: 0.8055556 
# Recall: 0.5631068 
# F-Score: 0.6628571 

# > modelMetrics(valid$Survived,valid$pred,0.6)
# actual FALSE TRUE
# 0   161    4
# 1    53   50
# Precision: 0.9259259 
# Recall: 0.4854369 
# F-Score: 0.6369427 

saveRDS(model.glm,file = paste0(path,"glm_v1.rds"))
