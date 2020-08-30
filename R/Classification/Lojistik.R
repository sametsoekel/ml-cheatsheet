
####

df <- Default

glimpse(df)

profiling_num(df)

plot_num(df)

freq(df)



##### train-test ayrımı ######

set.seed(42)
train_index <- createDataPartition(df$default,
                                   p = .8,
                                   list = F,
                                   times = 1)

train <- df[train_index,]

test <- df[-train_index,]


train_y <- train$default

train_x <- train %>% dplyr::select(-c("default"))

test_y <- test$default

test_x <- test %>% dplyr::select(-"default")


#############################


###### Doğrusal Regresyon ile sınıflandırma


head(train$default)

as.numeric(train$default)-1



## Model nesnesi

model_lm <- lm(as.numeric(train$default)-1 ~ balance, data = train)

summary(model_lm)

plot(model_lm)



options(scipen = 9)
############# MODEL KURMAK #############


model_glm <- glm(default~.,
                 data = train,
                 family = "binomial")



summary(model_glm)



coef(model_glm)




########## Tahmin Yapmak ##########


ol <- predict(model_glm,test_x,type="response") ### OLASILIK DEĞERLERİNİ TAHMİN ETTİ

summary(ol)


model_glm_pred <- ifelse(ol < 0.5,"No","Yes")

table(model_glm_pred)



#### Sınıflandırma hatası ####


class_err <- function (gercek,tahmin){
  
  mean(gercek != tahmin)
  
}


class_err(test$default,model_glm_pred) ## Yanlış sınıflandırma oranı

1-class_err(test$default,model_glm_pred) ## Doğruluk değeri


tb <- table(tahmin = model_glm_pred,
            gercek = test$default)




km <- confusionMatrix(tb, positive = "Yes") #### Süper ölçüm

km$overall["Accuracy"] ## Accuracy değeri

km$byClass["Sensitivity"] ## Belirleyicilik katsayısı




########## Tahminleri Görselleştirmek ##########


plot(as.numeric(train$default)-1~balance,data = train,
     col = "dodgerblue",
     pch = "I")

abline(h = 0, lty = 3)
abline(h = 1, lty = 3)
abline(h = 0.5, lty = 2)


model_glm <- glm(default~balance,
                 data = train,
                 family = "binomial")

curve(
  predict(model_glm,
              data.frame(balance = x),
              type = "orange"),
  add = TRUE,
  lwd = 3,
  col = "dodgerblue"
  )



### ROC eğrisi (pRoc)

model_glm <- glm(default~.,
                 data = train,
                 family = "binomial")

test_ol <- predict(model_glm, newdata = test_x, type = "response")



roc(test_y~test_ol,
    plot = T,
    print.auc = T) 


str(df$default)


########### MODEL TUNING.. ###########

ctrl <- trainControl(method = "cv", number = 10,
                     summaryFunction = twoClassSummary,
                     classProbs = T,
                     savePredictions = T)


glm_tune <- train(default~.,data=train,
                  method = "multinom",
                  trainControl = ctrl,
                  preProcess="scale") ## Multinom ?



test_tahmin <- predict(glm_tune,test_x)

###### Test hatası

defaultSummary(
  data.frame(
    obs = test_y,
    pred = test_tahmin
  )
)



###### Karmaşıklık matrisi


tb <- table(tahmin = test_tahmin,
            gercek = test$default)

km <- confusionMatrix(tb, positive = "Yes")

km