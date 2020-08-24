df <- Hitters
df <- na.omit(df)

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

test_x <- test %>% dplyr::select(-"Salary")

train_x_x <- train_x %>%
  dplyr::select(-c("League","NewLeague","Division"))

test_x_x <- test_x %>% 
  dplyr::select(-c("League","NewLeague","Division"))



##### Model Kurmak #####
## e1071

svm_fit <- svm(train_x_x,train_y)

svm_fit ## çıktı

svm_fit$epsilon

svm_fit$cost




##### Tahmin Yapmak #######

predicted <- predict(svm_fit,test_x_x)


hatalar <- data.frame(obs = test_y,
                      pred = predicted)

defaultSummary(hatalar)






###### Model Tuning #######


ctrl <- trainControl(method = "cv", number = 10)


set.seed(72)



svm_tune <- train(train_x_x,train_y,
                   method = "svmRadial",
                   trControl = ctrl,
                   tuneLength = 14,
                   preProc = c("center","scale"))


plot(svm_tune)


svm_tune$finalModel




#### Tune edilmiş modelden test hatası #######

predicted <- predict(svm_tune,test_x_x)


hatalar <- data.frame(obs = test_y,
                      pred = predicted)

defaultSummary(hatalar)


