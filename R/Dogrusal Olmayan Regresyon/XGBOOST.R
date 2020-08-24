

##### Gradient Boosting'in heyecanlı torunu ########


### drat library

install_github("eddelbuettel/drat")

drat::addRepo("dmlc")

install_github("dmlc/xgboost/tree/master/R-package")

install.packages('xgboost')



#######

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

########







############## Model Kurmak ############

xgb_fit <- xgboost(data = as.matrix(train_x),
                   label = train_y,
                   booster = "gblinear",
                   max.depth = 2,
                   eta = 1,
                   nthread = 2,
                   nrounds = 1000)



### XGB'nin en verimli çalıştığı kendi veri tipi

dtrain <- xgb.DMatrix(data = as.matrix(train_x),
                      label = train_y)

dtest <- xgb.DMatrix(data = as.matrix(test_x),
                      label = test_y)


### bir de onunla deneyelim

xgb_fit <- xgboost(data = dtrain,
                   booster = "gblinear",
                   max.depth = 2,
                   eta = 1,
                   nthread = 2,
                   nrounds = 1000,
                   verbose = 1) ###verbose göstere göstere gidiyor


imp_matris <- xgb.importance(model = xgb_fit)

##### Değişken önem düzeyleri

xgb.plot.importance(imp_matris)

## Bu da plot'u




##### Overfitting'e karşı watchlist

#### TEST HATASINI DA GÖSTERİYOR SÜPPERRRRR


watchlist <- list(train = dtrain, test = dtest)

xgb_fit2 <- xgb.train(data = dtrain,
                      booster = "gblinear",
                      max.depth = 2,
                      eta = 1,
                      nthread = 2,
                      nrounds = 2,
                      watchlist = watchlist
                      )


###### XGB TAHMİN ########


pred <- predict(xgb_fit, as.matrix(test_x))

plot(pred,test_y,
     xlab="Tahmin",
     ylab="Gerçek",
     pch = 20)
abline(0,1)



defaultSummary(
  data.frame(
    pred = pred,
    obs = test_y
  )
)   ### Tune edilmediği için sonuçlar leş




############ Model Tuning #############


ctrl <- trainControl(method = "cv", number = 10)

xgb_grid <- expand.grid(
  nrounds = 1000,
  lambda = c(1,2,3),
  alpha = c(0, 0.5, 0.1),
  eta = c(0, 0.5, 1)
)
## lambda cezalandırma terimi
## ngrounds en az 1000 öneriliyor
## eta learning rate

xgb_tune <- train(
  x=data.matrix(train_x),
  y=train_y,
  trControl=ctrl,
  tuneGrid = xgb_grid,
  method = "xgbLinear"
)


pred <- predict(xgb_tune, as.matrix(test_x))

defaultSummary(
  data.frame(
    pred = pred,
    obs = test_y
  )
)





######## SÜPER GELİŞMİŞ HİPERPARAMETRE OPTİMİZASYONU #####

### xgboost'u linear değil de ağaç yapısıyla optimize etme



grid_default <- expand.grid(
  
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

###### Öncelikle ön tanımlı değerlerle grid oluşturdum


ctrl <- trainControl(method = "none",
                     verboseIter = F,
                     allowParallel = T)


xgb_base <- train(
  
  x = as.matrix(train_x),
  y = train_y,
  trControl = ctrl,
  tuneGrid = grid_default,
  method = "xgbTree",
  verbose = T
)


pred <- predict(xgb_base, as.matrix(test_x))

defaultSummary(
  data.frame(
    pred = pred,
    obs = test_y
  )
)






########## Adım 1. İterasyon sayısı ve Learning Rate belirlenmesi


nrounds <- 1000

tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = nrounds, by = 50), 
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

ctrl <- trainControl(
  method = "cv", 
  number = 10, 
  verboseIter = FALSE, 
  allowParallel = TRUE  
)


xgb_tune <- train(
  x = as.matrix(train_x),
  y = train_y,
  trControl = ctrl,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE
)


defaultSummary(data.frame(obs = test_y, 
                          pred = predict(xgb_tune, as.matrix(test_x))))

##### Tuning işleminin görselleştirilmesi


tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$RMSE, probs = probs), min(x$results$RMSE))) +
    theme_bw()
}


tuneplot(xgb_tune)

xgb_tune$bestTune







##### Bu tuning işleminden dönen optimal değerleri adım2 de
#### kullanacağım (yeniden bir aralık belirleyerek)
### (daraltarak)






########### Adım 2 Maksimum Derinik, Minimum Child ######



tune_grid2 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = 4, ## burayı bi önceki adımdan aldım ama yeniden bir aralık yazdım, 4 optimal verdi.
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = c(1, 2, 3),
  subsample = 1
)


xgb_tune2 <- train(
  x = as.matrix(train_x),
  y = train_y,
  trControl = ctrl,
  tuneGrid = tune_grid2,
  method = "xgbTree",
  verbose = TRUE
)


tuneplot(xgb_tune2)

xgb_tune2$bestTune

defaultSummary(data.frame(obs = test_y, 
                          pred = predict(xgb_tune2, as.matrix(test_x))))














########## Adım 3 Değişken Ve Gözlem Örneklemesi ##############

tune_grid3 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = 4, 
  gamma = 0,
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0), 
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = c(0.5, 0.75, 1.0) 
)

xgb_tune3 <- train(
  x = as.matrix(train_x),
  y = train_y,
  trControl = ctrl,
  tuneGrid = tune_grid3,
  method = "xgbTree",
  verbose = TRUE
)


tuneplot(xgb_tune3)


xgb_tune3$bestTune
defaultSummary(data.frame(obs = test_y, 
                          pred = predict(xgb_tune3, as.matrix(test_x))))




## Adim 4: Gamma

tune_grid4 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = 5,
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune4 <- train(
  x = as.matrix(train_x),
  y = train_y,
  trControl = ctrl,
  tuneGrid = tune_grid4,
  method = "xgbTree",
  verbose = TRUE
)

tuneplot(xgb_tune4)
xgb_tune4$bestTune

defaultSummary(data.frame(obs = test_y, 
                          pred = predict(xgb_tune4, as.matrix(test_x))))




####### Adim 5: Learning Rate'in Indirgenmesi


tune_grid5 <- expand.grid(
  nrounds = seq(from = 100, to = 1000, by = 50),
  eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = xgb_tune4$bestTune$gamma,
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune5 <- train(
  x = as.matrix(train_x),
  y = train_y,
  trControl = ctrl,
  tuneGrid = tune_grid5,
  method = "xgbTree",
  verbose = TRUE
)

tuneplot(xgb_tune5)

xgb_tune5$bestTune
defaultSummary(data.frame(obs = test_y, 
                          pred = predict(xgb_tune5, test_x)))





######## Adim 6: Son Model


final_grid <- expand.grid(
  nrounds = xgb_tune5$bestTune$nrounds,
  eta = xgb_tune5$bestTune$eta,
  max_depth = xgb_tune5$bestTune$max_depth,
  gamma = xgb_tune5$bestTune$gamma,
  colsample_bytree = xgb_tune5$bestTune$colsample_bytree,
  min_child_weight = xgb_tune5$bestTune$min_child_weight,
  subsample = xgb_tune5$bestTune$subsample
)


xgb_son_model <- train(
  x = as.matrix(train_x),
  y = train_y,
  trControl = ctrl,
  tuneGrid = final_grid,
  method = "xgbTree",
  verbose = TRUE
)


#test hatasi
defaultSummary(data.frame(obs = test_y, 
                          pred = predict(xgb_son_model, as.matrix(test_x))))




######### Model Kaydetmek ve Paylaşmak #######


save(xgb_son_model, file ="xgb_son_model.rda")





