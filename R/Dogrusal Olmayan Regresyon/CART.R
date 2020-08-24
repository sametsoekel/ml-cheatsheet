df <- read_csv("C:/Users/user/Desktop/R/Dogrusal Olmayan Regresyon/Advertising.csv", 
               +     col_types = cols(X1 = col_skip()))

df <- na.omit(df)

set.seed(42)
train_index <- createDataPartition(df$Sales,
                                   p = .8,
                                   list = F,
                                   times = 1)

train <- df[train_index,]

test <- df[-train_index,]


train_y <- train$Sales

train_x <- train %>% dplyr::select(-"Sales")

test_y <- test$Sales

test_x <- test %>% dplyr::select(-"Sales")





######## Model Kurmak #########


cart_tree <- rpart(Sales~TV,data=df)


cart_tree #### Kural seti


cart_tree$variable.importance ## Değişken önemleri


plot(cart_tree, margin = 0.1)
text(cart_tree, cex = 0.5) ### Süper ağaç görselleştirme



prp(cart_tree, type=4) ### Buda görselleştirme şeysi


rpart.plot(cart_tree) #### En tatlı görselleştirme


plotcp(cart_tree) ### Karmaşıklık parametresinin değişimi


### GGplot ile görselleştirme

df %>%
  mutate(y_sapka = predict(cart_tree)) %>% 
  ggplot()+
  geom_point(aes(TV,Sales))+
  geom_step(aes(TV,y_sapka), col ="red")



### Karmaşıklık parametresi ve minsplit

cart_tree <- rpart(Sales~TV,data=df,
                   control=rpart.control(cp=0,
                                         minsplit = 3))


### Overfitting örneği

df %>%
  mutate(y_sapka = predict(cart_tree)) %>% 
  ggplot()+
  geom_point(aes(TV,Sales))+
  geom_step(aes(TV,y_sapka), col ="red") ### Overfitting örneği


rpart.plot(cart_tree)




##### Ağacı budamak

budanmis_cart <- prune(cart_tree, cp=0.01)

df %>%
  mutate(y_sapka = predict(budanmis_cart)) %>% 
  ggplot()+
  geom_point(aes(TV,Sales))+
  geom_step(aes(TV,y_sapka), col ="red") 


plot(budanmis_cart, margin = 0.1)
text(budanmis_cart, cex = 0.5)




######## Tahmin ############


pred <- predict(budanmis_cart,test_x)

defaultSummary(
  data.frame(
    pred=pred,
    obs=test_y
  )
)






######## Model Tuning ##########


ctrl <- trainControl(method = "cv", number = 10)

tune_grid <- data.frame(
  cp=seq(0,0.05,len=25)
)


cart_tune <- train(train_x,train_y,
                  method = "rpart",
                  trControl = ctrl,
                  tuneGrid = tune_grid,
                  preProc = c("center","scale"))

rpart.plot(cart_tune$finalModel)

## Test hatasına bir bakalım
pred <- predict(cart_tune,test_x)

defaultSummary(
  data.frame(
    pred=pred,
    obs=test_y
  )
)

#### Tahmin ve gerçek değerlerin karşılaştırılması

plot(pred,test_y,
     xlab="Tahmin",ylab="Gerçek")
abline(0,1)




### partykit ile gösterim


plot(as.party(cart_tune$finalModel)) #### Süper
