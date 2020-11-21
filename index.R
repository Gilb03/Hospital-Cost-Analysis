library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
setwd("~/zrc/HHS_ProviderRelief_Analysis/data")


#READ IN DATA
HHSindata <- read.csv("rows.csv")
hosp_cost <- read.csv("HospitalCosts.csv")

#DESCRIPTIVE STATS 
str(hosp_cost)
head(hosp_cost)
tail(hosp_cost)
summary(hosp_cost)


head(hosp_cost$AGE)
summary(hosp_cost$AGE)
table(hosp_cost$AGE)
hist(hosp_cost$AGE)
summary(as.factor(hosp_cost$AGE))
max(table(hosp_cost$AGE))
max(summary(as.factor(hosp_cost$AGE)))
which.max(table(hosp_cost$AGE))
age <- aggregate(TOTCHG ~ AGE, data = hosp_cost, sum)
max(age)


str(hosp_cost)
head(hosp_cost$RACE)
tail(hosp_cost$RACE)
summary(hosp_cost$RACE)
plot(hosp_cost$RACE)
summary(as.factor(hosp_cost$RACE))
max(summary(as.factor(hosp_cost$RACE)))


str(hosp_cost$FEMALE)
head(hosp_cost$FEMALE)
summary(hosp_cost$FEMALE)
tail(hosp_cost$FEMALE)
hist(hosp_cost$FEMALE)


#In order of severity of the diagnosis and treatments and to find out the expensive treatments, the agency wants to find the diagnosis related group that has maximum hospitalization and expenditure.
t <- table(hosp_cost$APRDRG)
d <- as.data.frame(t)
names(d)[1] = 'Diagnosis Group'
d
which.max(table(hosp_cost$APRDRG))
which.max(t)
which.max(d)          
res <- aggregate(TOTCHG ~ APRDRG, data = hosp_cost, sum)
res
which.max(res$TOTCHG)
res[which.max(res$TOTCHG),]

#To make sure that there is no malpractice, the agency needs to analyze if the race of the patient is related to the hospitalization costs
table(hosp_cost$RACE)
hosp_cost$RACE <- as.factor(hosp_cost$RACE)
fit <- lm(TOTCHG ~ RACE,data=hosp_cost)
fit
summary(fit)
fit1 <- aov(TOTCHG ~ RACE,data=hosp_cost)
summary(fit1)
hosp_cost <- na.omit(hosp_cost)

#To properly utilize the costs, the agency has to analyze the severity of the hospital costs by age and gender for proper allocation of resources.
table(hosp_cost$FEMALE)
a <- aov(TOTCHG ~ AGE+FEMALE,data=hosp_cost)
summary(a)
b <- lm(TOTCHG ~ AGE+FEMALE,data=hosp_cost)
summary(b)

#Since the length of stay is the crucial factor for inpatients, the agency wants to find if the length of stay can be predicted from age, gender, and race.
table(hosp_cost$LOS)
cat <- aov(LOS ~ AGE+FEMALE+RACE,data=hosp_cost)
summary(cat)
cat <- lm(LOS ~ AGE+FEMALE+RACE,data=hosp_cost)
summary(cat)

#To perform a complete analysis, the agency wants to find the variable that mainly affects the hospital costs.
aov(TOTCHG ~.,data=hosp_cost)
mod <- lm(TOTCHG ~ .,data=hosp_cost)
summary(mod)





#RANDOM FOREST 

set.seed(1937028)
train_ind <- sample(nrow(hosp_cost),round(0.75*nrow(hosp_cost)))
train     <- hosp_cost[train_ind,]
test      <- hosp_cost[-train_ind,]

str(hosp_cost)
rfModel <- randomForest(RACE ~ . , data = hosp_cost)
test$predicted <- predict(rfModel, test)
library(caret)
confusionMatrix(test$RACE, test$predicted)


library(MLmetrics)
F1_all <- F1_Score(test$RACE, test$predicted)
F1_all

options(repr.plot.width=5, repr.plot.height=4)
varImpPlot(rfModel,
           sort=T,
           n.var=10,
           main="Most Important Variables")


#KNN does not work
str(hosp_cost)
hosp_cost$Class <- as.factor(hosp_cost$Class)
set.seed(1991)
samp <- sample(1:nrow(hosp_cost), round(0.2*nrow(hosp_cost)))
cost <- hosp_cost[samp, ]
index <- createDataPartition(cost$RACE, p = 0.75, list = F)
train <- cost[index, ]
test <- cost[-index, ]

library(caret)
library(class)
knn1 <- knn(train = train[,-31], test = test[,-31], cl = train$Class, k = 3)
confusionMatrix(knn1, test$Class, positive = "1")
