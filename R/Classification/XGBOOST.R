url <- "http://nrvis.com/data/mldata/pima-indians-diabetes.csv"

df <- read_csv(file = url,
               col_names = c("pregnant","glucose","pressure","triceps","insulin","mass","pedigree","age","diabetes"))


df$diabetes <- as.factor(df$diabetes)

plot_num(df)

freq(df)



#### dataset #####



set.seed(42)
train_index <- createDataPartition(df$diabetes,
                                   p = .8,
                                   list = F,
                                   times = 1)

train <- df[train_index,]

test <- df[-train_index,]



train_y <- train$diabetes

train_x <- train %>% dplyr::select(-c("diabetes"))

test_y <- test$diabetes

test_x <- test %>% dplyr::select(-"diabetes")


##### dataset ########



View(train_y)


######### MOdel Kurmak ##########


dtrain <- xgb.DMatrix(data = as.matrix(train_x), label = as.numeric(train_y)-1)


dtest <- xgb.DMatrix(data = as.matrix(test_x), label = as.numeric(test_y)-1)

## Kendi veri tipinde kaydediyoruz


xgb_fit <- xgboost(data = dtrain,
                   max.depth = 2,
                   eta = 1,
                   ntread = 2,
                   nrounds = 20,
                   objective = "binary:logistic",
                   verbose = 1)

##### Nedense factor olunca modeli kurmuyor ?


bst <- xgb.train(data=dtrain,
                 max.depth = 2,
                 eta = 1,
                 ntread = 2,
                 nrounds = 20,
                 eval.metric = "error",
                 eval.metric = "logloss",
                 objective = "binary:logistic")



####### Değişken önem düzeyleri


mm <- xgb.importance(model = bst)

xgb.plot.importance(mm)



#### Ağa. yapısının görülmesi

xgb.dump(bst, with_stats = T) ## Kural yapısı


## Görsel için DiagrammeR package gerekli


xgb.plot.tree(model = bst) ## Görsel





########### Tahmin Yapmak ############

confusionMatrix(predict(bst, dtest, type="class")
                ,test_y, positive ="1")






####### Model Tuning ##########


ctrl <- trainControl(method = "cv",
                     number = 10,
                     summaryFunction = twoClassSummary,
                     classProbs = T)

xgb_grid <- expand.grid(eta = c(0.05,0.03,0.02),
                        nrounds = c(50, 75, 100),
                        max_depth = 1:7,
                        min_child_weight = c(2.0,2.25),
                        colsample_bytree = c(0.3,0.4,0.5),
                        gamma = 0,
                        subsample = 1)

dim(xgb_grid)


levels(train$diabetes) <- make.names(
  levels(
    factor(
      train$diabetes
    )
  )
) #### Bu boktan şeyi yeniden yapmalıyız, hiç bir mantığı yok



xgb_tune <- train(diabetes~.,data=train,
                  method="xgbTree",
                  tuneGrid = xgb_grid,
                  trControl = ctrl,
                  metric = "ROC")



xgb_tune$bestTune


pred <- predict(xgb_tune,test_x)

obs <- test_y

levels(obs) <- make.names(
  levels(
    factor(
      test_y
    )
  )
) 



####### TEST HATASI ######


confusionMatrix(pred,obs,positive = "X1")


#### %73 DOĞRULUK VERDİ



