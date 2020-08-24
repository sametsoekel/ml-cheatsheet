

###### Yüksek korelasyonlu gruplar olduğunda önerilir #####

## Lasso ve ridge regresyon karışımı

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


train_x_x <- train_x %>%
  select(-c("League","NewLeague","Division"))

test_x_x <- test_x %>% 
  select(-c("League","NewLeague","Division"))

###### Model Kurmak ###########

## elasticnet kütüphanesinden enet

enet_fit <- enet(x = as.matrix(train_x_x), y = train_y,
                 lambda = 1,
                 normalize = T)



#### Tahmin yapmak ####

predict(enet_fit,
        newx =as.matrix(test_x_x),
        s = 1,
        mode = "fraction",
        type = "fit")

## Katsayıları da şöyle elde ediyoruz


predict(enet_fit,
        newx =as.matrix(test_x_x),
        s = 1,
        mode = "fraction",
        type = "coefficients")



####### Model Tuning ########



ctrl <- trainControl(method = "cv", number = 10)



set.seed(72)


enet_grid <- data.frame(
  lambda = seq(0,.1,length = 20),
  fraction = seq(0.05,1, length = 20)
)

enet_tune <- train(train_x_x,train_y,
                    method = "enet",
                    trControl = ctrl,
                    tuneGrid = enet_grid,
                    preProc = c("center","scale"))
plot(enet_tune)

enet_tune$bestTune

enet_tune$results %>% 
  filter(fraction == as.numeric(enet_tune$bestTune))

##### En iyi tune edilmiş parametreler

## En iyi modelin katsayıları;

enet_tune$finalModel



###### En iyi modelin test hatası #####

defaultSummary(
  data.frame(
    obs = test_y,
    pred = as.vector(predict(enet_tune,
                             as.matrix(test_x_x)))
  )
)


