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

train_x <- train %>% select(-c("Salary"))

test_y <- test$Salary 

test_x <- test %>% select(-"Salary")






###### Model Kurmak ##########

## pls kütüphanesi gereklidir





pls_fit <- plsr(Salary~., data = train)

summary(pls_fit)

validationplot(pls_fit,val.type = "MSEP")

pls_fit$coefficients

pls_fit$scores



###### Tahmin ? ######




predicted <- predict(pls_fit,test_x, ncomp = 1:2)

View(predicted)


##### Test hatası hesaplama ####

hatalar <- data.frame(test_y,
                      predicted)

colnames(hatalar) <- c("obs","pred")

defaultSummary(hatalar)




###### Tuning #######

ctrl <- trainControl(method = "cv", number = 10)
set.seed(72)

pls_tune <- train(train_x,train_y,
                  method = "pls",
                  trControl = ctrl,
                  tuneLength = 20,
                  preProc = c("center","scale"))

plot(pls_tune)

pls_tune$results

