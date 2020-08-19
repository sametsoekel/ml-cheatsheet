##### Tahmine Dayalý Deðer Atama Yöntemleri #####df <- Hitters
df <- Hitters

df[sample(1:nrow(df),7),"Hits"] <- NA
df[sample(1:nrow(df),9),"Runs"] <- NA
df[sample(1:nrow(df),5),"RBI"] <- NA


##DMwR kütüphanesi KNN ile atama yapmak için gerekli

anyNA(df$HmRun) #Tek bir eksik varsa bile T, F döndürüyor

knn_data <- knnImputation(df, k=5) #doldurdu tüm eksikleri

anyNA(knn_data)

knn_data <- knnImputation(df, k=5, meth="median") # medyan olarak knn uygular



##### Random Forests ile Atama Yapmak #####

### missForest gerekli bu iþlem için

df <- Hitters

df[sample(1:nrow(df),7),"Hits"] <- NA
df[sample(1:nrow(df),9),"Runs"] <- NA
df[sample(1:nrow(df),5),"RBI"] <- NA


rf_data <- missForest(df, ntree=7) # iþlem bu

#eksik deðerlerin indexleri

l <- sapply(df, function(x) which(is.na(x)))

