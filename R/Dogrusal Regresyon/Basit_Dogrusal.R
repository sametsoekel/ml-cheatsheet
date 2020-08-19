##### Required Lib #####

library(caret)
library(tidyverse)
library(AppliedPredictiveModeling)
library(pls) #kismi en kucuk kareler ve pcr icin
library(elasticnet)
library(broom) #tidy model icin
library(glmnet)
library(MASS)
library(ISLR)
library(PerformanceAnalytics)
library(funModeling)
library(Matrix)

##### Required Lib #####


df <- Advertising

glimpse(df) ## Betimsel istatistikler

summary(df) ## Değişkenlerin dağılımları


gg <- profiling_num(df) ## İstatistiksel bilgiler

sum(is.na(df)) ## Eksik gözlem kontrolü



#### TV Harcamaları ve Satış Arasindaki İlişki

plot(Sales ~ TV,data = df) ## TV ve satışlar arasındaki ilişki

pairs(df) ## Tüm değişkenlerin korelasyonları


chart.Correlation(df,histogram = T) ## Süper korelasyon grafiği



###### Sadece Bağımlı Değişkene Yönelik Scatter Plot ####


featurePlot(x = df[ , c("TV","Radio","Newspaper")], y = df$Sales)




##### Model Kurmak #######

lm(Sales ~ ., data=df)


summary(lm(Sales ~ ., data=df)) ## Model çıktısı

lm_model <- lm(Sales ~ TV + Radio, data=df)



##### Tahmin yapmak #######


head(predict(lm_model))


ornek_gozlem <- data.frame(TV=120,Radio=85)

predict(lm_model,ornek_gozlem)



#### Tahmin edilen değerler için güven aralığı ####

predict(lm_model, ornek_gozlem, interval="confidence",level = 0.99)

lm_model