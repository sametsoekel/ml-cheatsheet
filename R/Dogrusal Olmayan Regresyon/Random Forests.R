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




######## Bunu Çok Övüyorlar ########

#### Gözlem ve değişken seçimleri rastgele yapılıyor


### Süper diyolaaar





#### Model Kurmak ####






rf_fit <- randomForest(train_x,train_y,
                       importance = T)

importance(rf_fit)


varImpPlot(rf_fit)




######### Tahmin yapmak #######



## tahmin ve gerçeği karşılaştıralım


plot(pred,test_y,
     xlab="Tahmin",ylab="Gerçek",
     col="darkblue",pch=20)
abline(0,1,col="dodgerblue")


defaultSummary(
  data.frame(
    obs = test_y,
    pred = pred
  )
)





######### Tuning.... ##########



ctrl <- trainControl(method = "cv", number = 10)


set.seed(72)

ncol(train_x)/3  ##4.3 çıkıyor 4 e yuvarlayacağım
##bunu seçmesini bekliyoruz..

tune_grid <- expand.grid(
  mtry = c(2,3,4,5,10)
)

rf_tune <- train(train_x,train_y,
                  method = "rf",
                  trControl = ctrl,
                  tuneGrid = tune_grid,
                  preProc = c("center","scale"))


rf_tune

plot(rf_tune)


#### Tune edilmiş modelin test hataları

pred <- predict(rf_tune,test_x)

plot(pred,test_y,
     xlab="Tahmin",ylab="Gerçek",
     col="darkblue",pch=20)
abline(0,1,col="dodgerblue")


defaultSummary(
  data.frame(
    obs = test_y,
    pred = pred
  )
)










######### Caret ile Random Search ######


ctrl <- trainControl(method = "cv", number = 10,
                     search = "random")

rf_tune <- train(train_x,train_y,
                 method = "rf",
                 trControl = ctrl,
                 tuneLength = 5,
                 preProc = c("center","scale"))

rf_tune ### Kendisi rastgele denedi ve 6 optimal değişken seçti







###### Caret ile Grid Search ######


ctrl <- trainControl(method = "cv", number = 10,
                     search = "grid")

tune_grid <- expand.grid(
  mtry = 1:10
)


#### Ağaç sayısını da tune edelim
### biraz zor oluyor


model_listesi <- list()

for (ntree in c(100,200,300,500,1000,2000)){
  
  set.seed(111)
  
  fit <- train(train_x,train_y,
               method = "rf",
               trControl = ctrl,
               tuneGrid = tune_grid,
               ntree = ntree,
               preProc = c("center","scale"))
  
  key <- toString(ntree)
  model_listesi[[key]] <- fit
  
}



### Uzun sürdüğü için bunu çalıştırmadım, listedeki
## değerlere şöyle erişebiliriz

sonuclar <- resamples(model_listesi)
summary(sonuclar)




