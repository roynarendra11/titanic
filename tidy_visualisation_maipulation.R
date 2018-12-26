
path <- "D://kaggle//titanic//"

dir()

lapply(c('dplyr','ggplot2','purrr','magrittr','VIF','readr','tidyr','stringr'),require,character.only = T)



train <- read_csv(paste0(path,"train.csv")) 

glimpse(train)

sapply(train,function(x) sum(is.na(x)))

# converting charater variables to factors
# train %<>% mutate_if(is.character,as.factor)

train %<>% mutate(Survived=as.factor(Survived))
train$Pclass <- as.factor(train$Pclass) 

# Data Exploration --------------------------------------------------------


table(train$Survived) %>% prop.table()*100

# Respose rate

# 0        1 
# 61.61616 38.38384 


# Analysing Pclass

sum(is.na(train$Pclass))

table(train$Survived,train$Pclass) 

train %>% group_by(Pclass,Survived) %>% summarise(count = n()) %>% mutate(freq=count*100/sum(count)) %>% select(-count) %>% ungroup()%>% 
  spread(key = Survived, value = freq) %>% rename( "notSurvive" = `0`,"Survived" = `1`  )

train %>% group_by(Pclass,Survived) %>% summarise(count = n()) %>% mutate(freq=count*100/sum(count)) %>% select(-freq) %>% ungroup() %>% spread(key = Survived, value = count)


train %>% ggplot()+aes(x=Pclass,fill=Survived)+geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies")

#Conclusion: As class of people moves to common i.e class 1 to class 3, survival percentage within class decreases


# Creating a new variable: Married/Un-married

train %<>% mutate(mar_sts = case_when(
  grepl("Mr",Name) ~ "Married",
  grepl("Mrs",Name) ~ "Married",
  grepl("Miss",Name) ~ "Un-Married",
  grepl("Master",Name) ~ "Un-Married",
  TRUE ~ 'Others'
)) 

# Creating personal title from name
train$var1 <- sapply(strsplit(train$Name,","),"[",2)
train$per_title <- sapply(strsplit(train$var1,". "),"[",1)
train$var1 <- NULL

train$per_title <- case_when(train$per_title==' Mr' ~ "Mr",
                              train$per_title %in% c(' Mrs',' Lady') ~ "Mrs",
                              train$per_title %in% c(' Ms', ' Mme', ' Mlle', ' Miss') ~ "Miss",
                              train$per_title %in% c(' Master',' Jonkheer') ~ "Master",
                              train$per_title==' Dr' ~ "Dr",
                              TRUE ~ 'Others'
)
glimpse(train)

train %>% mutate(mar_sts = as.factor(mar_sts), per_title = as.factor(per_title)) %>% select(mar_sts,per_title) %>%  summary()

train %>% filter(per_title=='Others')

table(train$mar_sts,train$Survived)

# Relative to married and others, Un-married were saved more

table(train$mar_sts,train$per_title)

# Un-married population is mainly females whereas married population is mainly men

table(train$mar_sts == 'Married' & train$per_title == 'Mrs',train$Survived)
table(train$mar_sts == 'Others' & train$per_title == 'Mrs',train$Survived)

# Even in married population, majority of those who were saved were women

summary(train$Age[train$mar_sts=='Un-Married']) #unmarried women are young(Obviously!)
summary(train$Age[train$mar_sts=='Un-Married' & train$Parch >= 1])

table(train$Survived[train$mar_sts =='Un-Married' & train$Parch ==0])

table(train$Survived[train$mar_sts =='Un-Married' & train$SibSp ==0])

table(train$per_title,train$Survived)

# Females are saved more

summary(train$Age[train$mar_sts=='Married' & train$Sex=='female'])

summary(train$Age[train$mar_sts=='Married' & train$Survived==1])


glimpse(train)

# To see summary of Age, fare and parch and sibsp over different class
train %>% group_by(Pclass) %>% summarise_at(c("Age","Fare","Parch","SibSp"),funs(sum(.,na.rm = T),mean(.,na.rm = T),count = n(),na=sum(is.na(.)))) %>% View()

train %>% filter(Pclass==1) %>% mutate(flag_fare = ifelse(Fare>=mean(Fare,.na.rm=T),1,0)) %>% group_by(flag_fare,Survived) %>% 
  summarise(n()) %>% spread(key = Survived,value = `n()`)

# creating a flag of customers who paid more fare than average in each class

train %<>% group_by(Pclass) %>% mutate(flag_fare = ifelse(Fare >= mean(Fare,na.rm=T),1,0), flag_fare = as.factor(flag_fare))
glimpse(train)

table(train$flag_fare,train$Survived)

# Not a good variable: Drop suggested
train %<>% select(-flag_fare) 

train$Sex <- as.factor(train$Sex)
table(train$Survived,train$Sex)

#Females were significatly saved more than men
table(train$Survived, train$mar_sts)
table(train$Sex, train$mar_sts)
sum(is.na(train$mar_sts))
#Mostly unmarried girls and very young boys were saved and married women



train %>% filter(Sex == 'female') %>% ggplot()+aes(x=Survived, y=Age)+geom_boxplot()+facet_wrap(~per_title)

table(train$Pclass, train$Cabin)

train$Cabin1 <- substr(train$Cabin,1,1)
sum(is.na(train$Cabin1))
train %>% View()
#Mostly Na values..

train %<>% select(-Cabin1)

glimpse(train)

# Sibsp analysis
sum(is.na(train$SibSp))
train %>% ggplot()+aes(x=Survived, y=SibSp)+geom_boxplot() #not a good plot

table(train$Survived, train$SibSp)
train %>% group_by(SibSp,Survived) %>% summarise(n=n()) %>% mutate(freq = n*100/sum(n))%>% select(-n) %>% spread(key = Survived, value = freq)
# as the SibSp increases Survival rate decreases i.e People with more siblings or spouse on-board have low survival rate

glimpse(train)

# removing name

train$Name <- NULL

glimpse(train)

train %>% filter(Sex=='female') %>% ggplot()+aes(x=Survived,y=Age)+geom_boxplot()+facet_wrap(~Pclass)
# Elder women of class 1 but relatively younger women of class 2 and 3 were saved

train %>% filter(Sex=='male') %>% ggplot()+aes(x=Survived,y=Age)+geom_boxplot()+facet_wrap(~Pclass)
# Younger males were saved in 1 and 2, with no distinction in age for males in class 3

# impute missing age with regressed values: independent variables: Pclass, Sex, Fare, Parch, Sibsp

train.age <- train %>% filter(!is.na(Age)) %>% select(Pclass,Sex,Parch,SibSp,Age,Fare)
summary(train.age)

score.age <- train %>% filter(is.na(Age)) %>% select(PassengerId,Pclass,Sex,Parch,SibSp,Age,Fare)
summary(score.age)


model.age <- step(lm(Age~Sex+Pclass+SibSp+Fare+Pclass*Sex,data = train.age))
summary(model.age)

train.age$pred.age <- predict(model.age,newdata = train.age)

rmse = function(m, o){
  sqrt(mean((m - o)^2))
}

rmse(train.age$pred.age,train.age$Age)

score.age$Age1 <- predict(model.age,newdata = score.age) 

summary(score.age$Age1)

# Note: We do not have to explain it to anyone, had that been the case we would have done WOE transformation of age rather than doing regression substitution

glimpse(train)

train %>% select(-per_title,-Cabin,-Ticket) %>% left_join((score.age %>% ungroup() %>% select(PassengerId , Age1)),by = "PassengerId") %>% 
  mutate(Age = ifelse(is.na(Age),Age1,Age)) %>% select(-Age1) -> train 

glimpse(train)

train$mar_sts <- as.factor(train$mar_sts)
train$Embarked <- as.factor(train$Embarked)

glimpse(train)
summary(train)

table(train$Survived,train$Embarked)
table(train$Pclass,train$Embarked)

train$Embarked <- NULL

train %<>% ungroup() 

summary(train)
