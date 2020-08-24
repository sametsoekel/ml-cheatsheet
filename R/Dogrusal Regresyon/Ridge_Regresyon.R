### Yansız, basit, overfitting meyilli

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




######### Model Kurmak ##########

## glmnet kütüphanesi kullanılmalıdır


## Kategorik değişkenleri çıkarıyorum görselleştirirken zorluk çıkarmaması için

train_x_x <- train_x %>%
  select(-c("League","NewLeague","Division"))


ridge_fit <- glmnet(as.matrix(train_x_x),
                    y = train_y,
                    alpha = 0)

summary(ridge_fit)

ridge_fit$beta

plot(ridge_fit, xvar="lambda",label=T)



#### Minimum lambda değeri

ridge_fit$lambda %>% 
  log() %>% 
  min()


## Minimum lambda değerine karşılık gelen model katsayıları ??



### Doğru lambda değeri için cross validation yapılması


ridge_cv_fit <- cv.glmnet(as.matrix(train_x_x), y=train_y,
                          alpha = 0)

plot(ridge_cv_fit)

ridge_cv_fit$lambda.min

ridge_cv_fit$lambda.1se


#lambda değerlerine karşılık gelen katsayıları çekmek

coef(ridge_cv_fit,"lambda.min")


tidy(ridge_cv_fit)



###### Tahmin ######

###### GLMNET KÜTÜPHANESİ AÇILMALI !!!!!!!!

## kategorik değişkenleri yine siliyorum

test_x_x <- test_x %>% 
  select(-c("League","NewLeague","Division"))

defaultSummary(
  data.frame(
    obs = test_y,
    pred = as.vector(predict(ridge_cv_fit,
                             as.matrix(test_x_x)))
  )
)





########## Model Tuning ##########



ctrl <- trainControl(method = "cv", number = 10)


set.seed(72)


ridge_grid <- data.frame(
  lambda = seq(0,.1,length = 15)
)

ridge_tune <- train(train_x_x,train_y,
                  method = "ridge",
                  trControl = ctrl,
                  tuneGrid = ridge_grid,
                  preProc = c("center","scale"))


ridge_tune$bestTune

ridge_tune$results %>% 
  filter(lambda == as.numeric(ridge_tune$bestTune))

##### En iyi tune edilmiş parametreler

## En iyi modelin katsayıları;

ridge_tune$finalModel




###### En iyi modelin test hatası #####

defaultSummary(
  data.frame(
    obs = test_y,
    pred = as.vector(predict(ridge_tune,
                             as.matrix(test_x_x)))
  )
)
