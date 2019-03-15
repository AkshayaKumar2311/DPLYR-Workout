train <- read.csv("bank-full.csv",sep = ";")

install.packages("magrittr")
install.packages("dplyr")
library(dplyr)
library(magrittr)

meanMedian <- train %>% summarise_if(is.numeric,funs(n(),mean,median))

intsss <- train[sapply(train,is.numeric)]

sapply(train,class)

marriedornot <- train %>% filter(age>30) %>% group_by(job) %>% select(marital) 

table(marriedornot)

sum(is.na(train$job))

unique(train$job)

#####ACTUAL WORK OUT

dummyTrain <- train %>% distinct() %>% nrow() 

randomSample <- sample_n(train , size = nrow(train)*.8 , replace = F)

names(randomSample)

randomSample1 <- randomSample %>% rename("AGE" = "age" , "DAY" = "day")

lapply(randomSample , names)

summa <- train %>% select(-age , -job) #(-5:-9) 


train %>% select(starts_with("a")) %>% names
train %>% select(-starts_with("a")) %>% names
train %>% select(ends_with("b")) %>% names
train %>% select(contains("a")) %>% names
train %>% select(starts_with("c"),everything()) %>% names
train %>% select(matches("ous")) %>% names
####in ford data thiss will work 
#train %>% select(num_range(1:5)) %>% names
train %>% select(one_of("age","job")) %>% names

##Filter


train %>% filter(age>30) %>% nrow

train %>% filter(job %in% c("management","blue-collar")) %>% nrow

train %>% filter(job !="management" | job == "blue-collar") %>% nrow

train %>% filter(job !="management") %>% nrow

##SUMMARISE

train %>% filter(job == "management") %>% summarise(MeanAge = mean (age), MedianAge = median(age))

train %>% filter(job == "management") %>% summarise_at(vars(age,balance) , funs(n(),mean,median))

train %>% filter(job == "management") %>% summarise_at(vars(age,balance) , funs(n(),MeanCols = sum(is.na(.)),mean=mean(.,na.rm = T)))

train %>% filter(job == "management") %>% summarise_if(is.numeric , funs(n(),mean , median))


####ARRANGE
##This can also be done using SORT function
train %>% arrange(age)

train %>% arrange(desc(age))

#####GROUP BY

train %>% filter(age>30) %>% group_by(job)

train %>%  group_by(job) %>% summarise_at(vars(age,balance) , funs(n(),mean,median))

