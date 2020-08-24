#### Övülenler Arasında


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


####### Model Kurmak ########

gbm_fit <- gbm(medv~., data = train,
               distribution = "gaussian",
               n.trees = 10000,
               interaction.depth = 1,
               shrinkage = 0.001,
               cv.folds = 10) 


#### Ağaç sayısını arttırdım öğrenme katsayısını düşürdüm

gbm.perf(gbm_fit,method = "cv")


summary(gbm_fit) ## Değişken önem düzeyleri


defaultSummary(
  data.frame(
    pred = predict(gbm_fit,test_x),
    obs = test_y
  )
)


####### Tahmin ########

predicted <- predict(gbm_fit, test_x) ## ntrees kullanabiliyoruz

plot(test_y,predicted,
     xlab="Gerçek",
     ylab="Tahmin")
abline(0,1)








######## Model Tuning.... #######


ctrl <- trainControl(method="cv", 10,
              search ="grid")

tune_grid <- expand.grid(
  interaction.depth = seq(1,7,2),
  n.trees = seq(100, 1000, 50),
  n.minobsinnode = 10:20
)


gbm_tune <- train(train_x,train_y,
                  method="gbm",
                  trControl = ctrl,
                  tuneGrid = gbm_grid,
                  verbose = F)

### Uzun sürüyor çalıştırmıyorum. Tune edilmiş modeli döndürüyor..

