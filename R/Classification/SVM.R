
set.seed(42)

x <- matrix(rnorm(40),20 ,2)

y <- rep(c(-1,1),c(10,10))

x[y == 1,] <- x[y == 1, ] + 1


plot(x, col= y+3,
     pch =19)

df <- data.frame(x, y=as.factor(y))




######## Model Kurmak (e1071) #########

svm_fit <- svm(y~., data = df,
               kernel = "linear", cost = 10,
               scale = F)


summary(svm_fit)



plot(svm_fit, df)




######### Tahmin


predict(svm_fit)


df$y <- ifelse(df$y==-1,0,1)
svm_fit$fitted <- ifelse(svm_fit$fitted==-1,0,1)

class_err <- function(gercek,tahmin){
  
  mean(gercek != tahmin)
  
}

1-class_err(df$y,svm_fit$fitted)


tb <- table(svm_fit$fitted, df$y)


confusionMatrix(tb, positive="1")




####### DoğrysaL olMayan SVM #######

setwd("C:/Users/user/Desktop/R/Classification")
load(file ="ESL.mixture.rda")


df <- ESL.mixture

attach(df)



plot(x,col = y+1)

df<- data.frame(y = factor(y),x)


###### Model Kurmak #######


n_svm_fit <- svm(y~.,
                 data=df,
                 scale = F,
                 kernel = "radial",
                 cost = 5)

n_svm_fit




######### Model Tuning ###########

data("segmentationData")

df <- segmentationData

glimpse(df)





svm_train <- df %>% 
  filter(Case=="Train") %>% 
  select(-Case)

svm_test <- df %>% 
  filter(Case=="Test") %>% 
  select(-Case)



svm_train_x <- svm_train %>% 
  select(-Class)

svm_train_y <- svm_train$Class

svm_test_x <- svm_test %>% 
  select(-Class)

svm_test_y <- svm_test$Class



#############################



set.seed(42)

ctrl <- trainControl(method = "cv", number =10,
                     summaryFunction = twoClassSummary,
                     classProbs = T)

svm_grid <- expand.grid(
  sigma = c(0.01,0.015,0.2),
  C = c(0.75, 0.9, 1, 1.1, 1.25)
)


svm_tune <- train(svm_train_x,svm_train_y,
                  method = "svmRadial",
                  metric = "ROC",
                  tuneGrid = svm_grid,
                  trControl = ctrl)


plot(svm_tune)



#### Test hatası #####


confusionMatrix(predict(svm_tune,svm_test_x),
                svm_test_y, positive = "WS")




##### Olasılıkların hesaplanması



predict(svm_tune,svm_test_x, type="prob")

 






