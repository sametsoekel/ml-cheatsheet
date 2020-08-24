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



#### Model Kurma ####

## pls diye bir kütüphanenin içinde..

pcr_fit <- pcr(Salary~.,
              data=train,
              scale=T,
              validation = "CV")


######### Model Bitti ##########

summary(pcr_fit)

validationplot(pcr_fit, val.type = "MSEP") ## Bileşen sayısına karşılık hata değerleri

hatalar_frame <- data.frame(train_y,predict(pcr_fit, train_x))

colnames(hatalar_frame) <- c("obs","pred")


defaultSummary(hatalar_frame)


##### Tahmin #####

predict(pcr_fit, test_x[1:10,], ncomp = 1:2)

## ilk 10 gözlemi bir ve iki bileşen kullanarak tahmin et dedim.

##### Test hatası hesaplama #####


test_hatalar <- data.frame(test_y,predict(pcr_fit, test_x, ncomp = 3))
names(test_hatalar) <- c("obs","pred")

defaultSummary(test_hatalar)



#### En az hatalı model için tuning... #####


ctrl <- trainControl(method = "cv", number = 10)

set.seed(43)

pcr_tune <- train(train_x,train_y,
                  method="pcr",
                  trControl = ctrl,
                  tuneLength = 20,
                  preProc = c("center","scale"))

## train fonksiyonuna maksimum 20 değişkene kadar denemesini söyledim..


pcr_tune ## oda bana 16 optimaldir dedi


plot(pcr_tune) ## başka bir bakış açısı


pcr_tune$bestTune

pcr_tune$finalModel


### Son modelle test hatası inceleme

test_hatalar <- data.frame(test_y,predict(pcr_tune, test_x))
names(test_hatalar) <- c("obs","pred")

defaultSummary(test_hatalar)
