url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/haberman/haberman.data"

df <- read.csv(file = url,
               col.names = c("Age","Operation_Year","Number_Pos_Nodes","Survival"))


df$Survival <- ifelse(df$Survival ==2,0,1)
df$Survival <- as.factor(df$Survival)

summary(df)


ggpairs(df)


Freq(df$Survival) ## DescTools

freq(df$Survival) ## funModeling


scale01 <- function(x){
  
  (x-min(x))/(max(x)-min(x))
  
}



df <- df %>% mutate(Age = scale01(Age),
                    Operation_Year = scale01(Operation_Year),
                    Number_Pos_Nodes = scale01(Number_Pos_Nodes))


set.seed(42)
train_index <- createDataPartition(df$Survival,
                                   p = .7,
                                   list = F,
                                   times = 1)

train <- df[train_index,]

test <- df[-train_index,]

train$student <- as.numeric(train$Survival)-1

test$student <- as.numeric(test$Survival)-1


train_y <- train$Survival

train_x <- train %>% dplyr::select(-c("Survival"))

test_y <- test$Survival

test_x <- test %>% dplyr::select(-"Survival")


## bunu yapmak lazım

levels(train$Survival) <- make.names(
  levels(
    factor(
      train$Survival
    )
  )
)

train_y <- train$Survival





########### Model Kurmak ############
set.seed(42)

nnet_fit <- nnet(Survival~., df, size =3 ,decay =0.1) 


predict(nnet_fit,test_x) ### olasılık döndürüyor


pred <- predict(nnet_fit,test_x, type="class")
pred <- as.factor(pred)



confusionMatrix(pred, test_y, positive ="1")





########### Model Tuning ###########


ctrl <- trainControl(method ="cv",number=10,
                     summaryFunction = twoClassSummary,
                     classProbs = T)


nnetGrid <- expand.grid(size = 1:10,
                        decay = c(0,0.1,1,2))


maxSize <- max(nnetGrid$size)


numWts <- 1*(maxSize * length(train_x) + 1 + maxSize + 1)



nnet_tune <- train(
  train_x,train_y,
  method = "nnet",
  metric = "ROC",
  tuneGrid = nnetGrid,
  trace = F,
  maxit = 2000,
  MaxNWts = numWts,
  trControl = ctrl
)


nnet_tune$bestTune

plot(nnet_tune)

