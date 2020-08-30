df <- Default



set.seed(42)
train_index <- createDataPartition(df$default,
                                   p = .8,
                                   list = F,
                                   times = 1)

train <- df[train_index,]

test <- df[-train_index,]

train$student <- as.numeric(train$student)-1

test$student <- as.numeric(test$student)-1


train_y <- train$default

train_x <- train %>% dplyr::select(-c("default"))

test_y <- test$default

test_x <- test %>% dplyr::select(-"default")



###### Kategorik değişkenleri tanımadığı için nümerik 1,0 formatına indirgedim.




########## Model Kurmak(class lib) ########


knn_fit <- class::knn(train = train_x,
               test = test_x,
               cl = train_y,
               k = 5)



##### Tahmin ######

class_err <- function(gercek,tahmin){
  
  mean(gercek != tahmin)
  
}

class_err(test_y,knn_fit) ## Sınıflandırma hatası




##### Model Tuning ######




ctrl <- trainControl(method = "cv",
                     number = 10,
                     summaryFunction = twoClassSummary,
                     classProbs = T,
                     savePredictions = T)

knn_grid <- data.frame(
  k = c(4*(0:5)+1, 20*(1:5)+1, 50*(2:9)+1)
)


knn_tune <- train(train_x,train_y,
                  method = "knn",
                  metric = "ROC",
                  preProc = c("center","scale"),
                  trControl = ctrl,
                  tuneGrid = knn_grid
                  )


knn_tune$bestTune



confusionMatrix(
  knn_tune$pred$pred, knn_tune$pred$obs, positive = "Yes"
)