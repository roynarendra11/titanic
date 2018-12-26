

score.data <- read_csv(paste0(path,"test.csv")) 

glimpse(score.data)

sapply(score.data,function(x) sum(is.na(x)))

# converting charater variables to factors
# score.data %<>% mutate_if(is.character,as.factor)

score.data$Pclass <- as.factor(score.data$Pclass) 

score.data$Sex <- as.factor(score.data$Sex)

glimpse(score.data)

# Sibsp analysis
sum(is.na(score.data$SibSp))

score.data$Name <- NULL

glimpse(score.data)


# impute missing age with regressed values: independent variables: Pclass, Sex, Fare, Parch, Sibsp

score.data.age <- score.data %>% filter(!is.na(Age)) %>% select(Pclass,Sex,Parch,SibSp,Age,Fare)
summary(score.data.age)

score.data.age %<>% filter(!is.na(Fare))

score.age <- score.data %>% filter(is.na(Age)) %>% select(PassengerId,Pclass,Sex,Parch,SibSp,Age,Fare)
summary(score.age)


model.age <- step(lm(Age~Sex+Pclass+SibSp+Fare+Pclass*Sex,data = score.data.age))
summary(model.age)

score.data.age$pred.age <- predict(model.age,newdata = score.data.age)

rmse = function(m, o){
  sqrt(mean((m - o)^2))
}

rmse(score.data.age$pred.age,score.data.age$Age)


score.age$Age1 <- predict(model.age,newdata = score.age) 
summary(score.age$Age1)

# Note: We do not have to explain it to anyone, had that been the case we would have done WOE transformation of age rather than doing regression substitution

score.data %>% select(-Cabin,-Ticket) %>% left_join((score.age %>% ungroup() %>% select(PassengerId , Age1)),by = "PassengerId") %>% 
  mutate(Age = ifelse(is.na(Age),Age1,Age)) %>% select(-Age1) -> score.data 


glimpse(score.data)

score.data %<>% ungroup() 

summary(score.data)

score.data %<>% mutate_if(is.character,as.factor) 

score.data$pred <- predict(model.glm,newdata = score.data, type = "response")

score.data$Survived <-ifelse(score.data$pred>0.5,1,0) 
write.csv((score.data %>% select(PassengerId,Survived)),file = paste0(path,"predictions.csv"))

table(score.data$Survived)
