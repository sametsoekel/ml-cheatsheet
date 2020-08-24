
##### Lasso otomatik olarak değişken seçimi yapan bir algoritmadır ######

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


###### Model Kurmak ########

###### GLMNET kütüphanesiyle kuruyoruz

train_x_x <- train_x %>%
  select(-c("League","NewLeague","Division"))

lasso_fit <- glmnet(as.matrix(train_x_x),
                    y = train_y,
                    alpha = 1)

summary(lasso_fit)

lasso_fit$beta

plot(lasso_fit, xvar="lambda",label=T)

## Not : alpha 1 olunca Lasso cezalandırması uygulamış oluyoruz


tidy(lasso_fit)


lasso_fit$beta





######## Lambda seçimi için cross validation kullanılması #######


lasso_cv_fit <- cv.glmnet(as.matrix(train_x_x), y=train_y,
                          alpha = 1)

plot(lasso_cv_fit)

lasso_cv_fit$lambda.min

lasso_cv_fit$lambda.1se



### Model katsayıları

coef(lasso_cv_fit)

glance(lasso_cv_fit)






######## Tahmin ##########

test_x_x <- test_x %>% 
  select(-c("League","NewLeague","Division"))

defaultSummary(
  data.frame(
    obs = test_y,
    pred = as.vector(predict(lasso_cv_fit,
                             as.matrix(test_x_x)))
  )
)



###### Model Tuning ##########


ctrl <- trainControl(method = "cv", number = 10)


set.seed(72)


lasso_grid <- data.frame(
  fraction = seq(.05,.1,length = 20)
)

lasso_tune <- train(train_x_x,train_y,
                    method = "lasso",
                    trControl = ctrl,
                    tuneGrid = lasso_grid,
                    preProc = c("center","scale"))
plot(lasso_tune)

lasso_tune$bestTune

lasso_tune$results %>% 
  filter(fraction == as.numeric(lasso_tune$bestTune))

##### En iyi tune edilmiş parametreler

## En iyi modelin katsayıları;

lasso_tune$finalModel




###### En iyi modelin test hatası #####

defaultSummary(
  data.frame(
    obs = test_y,
    pred = as.vector(predict(lasso_tune,
                             as.matrix(test_x_x)))
  )
)


lasso_tune