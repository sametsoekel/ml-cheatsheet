df <- read_table(file = 'http://archive.ics.uci.edu/ml/machine-learning-databases/00243/yacht_hydrodynamics.data',
                 col_names = c('longpos_cob', 
                                'prismatic_coeff', 
                                'len_disp_ratio', 
                                'beam_draut_ratio', 
                                'length_beam_ratio',
                                'froude_num', 
                                'residuary_resist'))


glimpse(df)

profiling_num(df)



##### Yapay sinir ağları ölçeklenmiş değerlerde çalışıyor


olcek <- function(x) {
  
  (x-min(x)) / (max(x) - min(x))
  
}



df <- df %>%
  apply(2,olcek) %>% 
  data.frame()




ggpairs(df)

plot(df)

set.seed(42)
train_index <- createDataPartition(df$residuary_resist,
                                   p = .8,
                                   list = F,
                                   times = 1)

train <- df[train_index,]

test <- df[-train_index,]


train_y <- train$residuary_resist

train_x <- train %>% dplyr::select(-c("residuary_resist"))

test_y <- test$residuary_resist

test_x <- test %>% dplyr::select(-"residuary_resist")






###### Model Kurmak #########



## 1 katmanlı 1 nöronlu yapay sinir hücresi

ysa_fit <- neuralnet(residuary_resist~.,
                     data=train)

plot(ysa_fit)  ### süper



ysa_fit$result.matrix






###### Katman ve Nöron Sayısının Arttırılması ########


plot(neuralnet(residuary_resist~.,
          data=train,
          hidden = c(5,2)))



####### Değişkenlerin Önem ve Etki Düzeyleri ######

garson(ysa_fit) ### Önem düzeyi


lekprofile(ysa_fit) ## Etki düzeyi




##### Tahmin #########


### başta değişkenleri ölçeklemiştim, veri setini aynı
## set seed değeriyle yeniden çağırıyorumki gerçek 
# sonuçları alabileyim..

df <- read_table(file = 'http://archive.ics.uci.edu/ml/machine-learning-databases/00243/yacht_hydrodynamics.data',
                 col_names = c('longpos_cob', 
                               'prismatic_coeff', 
                               'len_disp_ratio', 
                               'beam_draut_ratio', 
                               'length_beam_ratio',
                               'froude_num', 
                               'residuary_resist'))

set.seed(42)
train_index <- createDataPartition(df$residuary_resist,
                                   p = .8,
                                   list = F,
                                   times = 1)

train <- df[train_index,]

test <- df[-train_index,]


train_y <- train$residuary_resist

train_x <- train %>% dplyr::select(-c("residuary_resist"))

test_y <- test$residuary_resist

test_x <- test %>% dplyr::select(-"residuary_resist")





defaultSummary(
  data.frame(obs = test_y,
             pred = predict(ysa_fit,test_x)))





######## Model Tuning #########



ctrl <- trainControl(method = "cv", number = 10)

ysa_grid <- expand.grid(
  decay = c(0.001,0.01,0.1),
  size = 1:10
)


ysa_tune <- train(train_x,train_y,
                  method = "mlpWeightDecay",
                  trControl = ctrl,
                  tuneGrid = ysa_grid,
                  preProc = c("center","scale"))


plot(ysa_tune)



### Tune edilmiş modelin test hatası


defaultSummary(
  data.frame(obs = test_y,
             pred = predict(ysa_tune,test_x)))