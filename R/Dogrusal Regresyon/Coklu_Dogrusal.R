df <- Hitters

df <- na.omit(df) ## Eksik gözlemleri sildim

rownames(df) <- NULL ## Satırlardaki oyuncu isimlerini sildim

## Caret kütüphanesinden değişkenleri ayırma, Test-Train


set.seed(42)
train_index <- createDataPartition(df$Salary,
                                   p = .8,
                                   list = F,
                                   times = 1)

## Train seti için %80 lik index oluşturdum


train <- df[train_index,]

test <- df[-train_index,]

# Train~Test veri setlerini ayırdım, Bağımlı ve bağımsız değişkenleri ayıracağım


train_y <- train %>% select("Salary")

train_x <- train %>% select(-c("Salary"))

test_y <- test %>% select("Salary")

test_x <- test %>% select(-"Salary")



##### Veriye bir bakalım

glimpse(train)

plot_num(train)

summary(train)

pairs(train %>% select(-c("League","NewLeague","Division")))

## Gelişmiş scatter plot

chart.Correlation(train %>% select(-c("League","NewLeague","Division")))




######## Model Kurmak ###########


lm_fit <- lm(Salary~ . , data = train)

summary(lm_fit)


### Caret ile hataları inceliyorum



hatas <- data.frame(train_y,lm_fit$fitted.values)

colnames(hatas) <- c("obs","pred")


defaultSummary(hatas) ### Hataları hesaplayan fonksiyon





#### Model ile tahmin #####

predict(lm_fit, test_x)


## Test hatasını inceliyorum

predicted_y <- predict(lm_fit, test_x)

test_hatas <- data.frame(test_y,predicted_y)
colnames(test_hatas) <- c("obs","pred")

defaultSummary(test_hatas)


##### Model Tuning....... ##########

## Caret kütüphanesinden fonksiyonlar

## 10 katlı cross validation nesnesi oluşturuyorum

ctrl <- trainControl(method = "cv",
                     number="10")

lm_val_fit <- caret::train(x = train_x,
                           y = train_y,
                           method = "lm", 
                           trControl = ctrl)

lm_val_fit ## Cross validation çıktısı

summary(lm_val_fit) ## Valide edilmiş model çıktısı

lm_val_fit$finalModel ## Final modeli !!




as.numeric(train_y$Salary)