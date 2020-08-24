

##### Required Lib ######


library(caret)
library(tidyverse)
library(AppliedPredictiveModeling)
library(pls) #kismi en kucuk kareler ve pcr icin
library(elasticnet)
library(broom) #tidy model icin
library(glmnet)
library(MASS)
library(ISLR)
library(PerformanceAnalytics)
library(funModeling)
library(Matrix) 
library(kernlab) #svm
library(e1071) #svm icin
library(rpart) #cart icin
library(pgmm) #olive data seti icin 
library(dslabs)
library(rpart.plot) #rpart gorsel icin
library(partykit) #karar agaci gorseli icin 
library(ipred) #bagging icin 
library(randomForest)
library(gbm)
library(nnet)
library(neuralnet)
library(GGally)
library(NeuralNetTools) #garson fonksiyonu icin
library(FNN)


##############################

df <- Hitters
df <- na.omit(df)

df <- df %>% 
  dplyr::select(-c("League","NewLeague","Division"))

set.seed(42)
train_index <- createDataPartition(df$Salary,
                                   p = .8,
                                   list = F,
                                   times = 1)

train <- df[train_index,]

test <- df[-train_index,]


train_y <- train$Salary

train_x <- train %>% dplyr::select(-c("Salary"))

test_y <- test$Salary 

test_x <- test %>% dplyr::select(-c("Salary"))

### Knn'de kategorik değişkenler olmamalıdır


###### Model Kurmak (fnn) ########


knn_fit <- knn.reg(train = train_x,
                   test = test_x,
                   y = train_y,
                   k=2)

ggplot(data=NULL,aes(knn_fit$pred,test_y))+
  geom_point()




######## Test hatası hesaplama #######

hatalar = data.frame(test_y,
                     knn_fit$pred)

names(hatalar) <- c("obs","pred")


defaultSummary(hatalar)



######## Model Tuning.. ###########


ctrl <- trainControl(method = "cv", number = 10)


set.seed(72)


knn_grid <- data.frame(
  .k = 1:20
)

knn_tune <- train(train_x,train_y,
                   method = "knn",
                   trControl = ctrl,
                   tuneGrid = knn_grid,
                   preProc = c("center","scale"))


        
plot(knn_tune)


hatalart = data.frame(test_y,
                     predict(knn_tune,test_x))

names(hatalart) <- c("obs","pred")


defaultSummary(hatalar)

