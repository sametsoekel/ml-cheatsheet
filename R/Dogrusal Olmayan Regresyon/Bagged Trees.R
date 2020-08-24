
#### Birden fazla ağacın ürettiği tahminlerin değerlendirilmesiyle
### öğrenmesi

## dataset = Boston

df <- Boston

set.seed(42)
train_index <- createDataPartition(df$medv,
                                   p = .8,
                                   list = F,
                                   times = 1)

train <- df[train_index,]

test <- df[-train_index,]


train_y <- train$medv

train_x <- train %>% dplyr::select(-c("medv"))

test_y <- test$medv

test_x <- test %>% dplyr::select(-"medv")




###### Model Kurmak #######


bag_fit <- ipredbagg(train_y,train_x)

names(bag_fit)

bag_fit$mtrees

#### Değişken sayısı sabitleyerek
### Random forest'i bagging regresyona indirgemek

bag_fit_2 <- randomForest(medv~., data=train,
             mtry = ncol(train)-1,
             importance = T,
             ntrees = 500)

plot(bag_fit_2)

bag_fit_2

importance(bag_fit_2)

varImpPlot(bag_fit_2) ## Önem düzeylerini plot etmek





######## Tahmin #########

pred <- predict(bag_fit_2, test_x)

defaultSummary(
  data.frame(
    pred = pred,
    obs = test_y
  )
)


### Bagging ağaç~hata karşılaştırılması

plot(bag_fit_2, col = "dodgerblue",
     lwd=2,
     main = "Hata ve Ağaç Sayisi Karsilastirimasi")



##### Model Tuning #####


ctrl <- trainControl(method = "cv", number = 10)


set.seed(72)

mtry <- ncol(train_x)

tune_grid <- expand.grid(
  .mtry = mtry
)

bag_tune <- train(train_x,train_y,
                   method = "rf",
                   trControl = ctrl,
                   tuneGrid = tune_grid)


bag_tune$bestTune


######## Tune edilmiş modelin Test hatası ########

defaultSummary(
  data.frame(
    obs = test_y,
    pred = predict(bag_tune,test_x)
  )
)
