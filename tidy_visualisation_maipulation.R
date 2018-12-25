
path <- "D://kaggle//titanic//"

dir()

lapply(c('dplyr','ggplot2','purrr','magrittr'),require,character.only = T)

library(readr)
library(stringr)
library(tidyr)

train <- read_csv(paste0(path,"train.csv")) 

glimpse(train)

train %<>% mutate(Survived=as.factor(Survived))
train$Pclass <- as.factor(train$Pclass) 

# Data Exploration --------------------------------------------------------


table(train$Survived) %>% prop.table()*100


# 0        1 
# 61.61616 38.38384 

table(train$Survived,train$Pclass) 
train %>% group_by(Pclass,Survived) %>% summarise(count = n()) %>% mutate(freq=count*100/sum(count)) %>% select(-count) %>% spread(key = Survived, value = freq)
train %>% group_by(Pclass,Survived) %>% summarise(count = n()) %>% mutate(freq=count*100/sum(count)) %>% select(-freq) %>% spread(key = Survived, value = count)


train %>% ggplot()+aes(x=Pclass,fill=Survived)+geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies")

# More People saved from Class 1 and 2 than 3

train %<>% mutate(mar_sts = case_when(
  grepl("Mr",Name) ~ "Married",
  grepl("Mrs",Name) ~ "Married",
  grepl("Miss",Name) ~ "Un-Married",
  grepl("Master",Name) ~ "Un-Married",
  TRUE ~ 'Others'
)) 

train$var1 <- sapply(strsplit(train$Name,","),"[",2)
train$per_title <- sapply(strsplit(train$var1,". "),"[",1)
train$var1 <- NULL

train$per_title1 <- case_when(train$per_title==' Mr' ~ "Mr",
                              train$per_title %in% c(' Mrs',' Lady') ~ "Mrs",
                              train$per_title %in% c(' Ms', ' Mme', ' Mlle', ' Miss') ~ "Miss",
                              train$per_title %in% c(' Master',' Jonkheer') ~ "Master",
                              train$per_title==' Dr' ~ "Dr",
                              TRUE ~ 'Others'
)


train %>% filter(mar_sts=='Others') %>% View()


glimpse(train)

train$Sex <- as.factor(train$Sex)
table(train$Survived,train$Sex)

#Relatively females were significatly more in survivors

table(train$Survived, train$mar_sts)
table(train$Sex, train$mar_sts)
sum(is.na(train$mar_sts))
#Mostly unmarried girls and very young boys were saved and married women



train %>% filter(Sex == 'female') %>% ggplot()+aes(x=Survived, y=Age)+geom_boxplot()+facet_wrap(~per_title1)

table(train$Pclass, train$Cabin)

train$Cabin1 <- substr(train$Cabin,1,1)
sum(is.na(train$Cabin1))
#Mostly Na values..

train %<>% select(-Cabin1)

glimpse(train)

sum(is.na(train$SibSp))
train %>% ggplot()+aes(x=Survived, y=SibSp)+geom_boxplot()
table(train$Survived, train$SibSp)
# as the SibSp increases survival rate decreases

