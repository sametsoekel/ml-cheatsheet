##### Tahmine Dayal� De�er Atama Y�ntemleri #####df <- Hitters
df <- Hitters

df[sample(1:nrow(df),7),"Hits"] <- NA
df[sample(1:nrow(df),9),"Runs"] <- NA
df[sample(1:nrow(df),5),"RBI"] <- NA


##DMwR k�t�phanesi KNN ile atama yapmak i�in gerekli

anyNA(df$HmRun) #Tek bir eksik varsa bile T, F d�nd�r�yor

knn_data <- knnImputation(df, k=5) #doldurdu t�m eksikleri

anyNA(knn_data)

knn_data <- knnImputation(df, k=5, meth="median") # medyan olarak knn uygular



##### Random Forests ile Atama Yapmak #####

### missForest gerekli bu i�lem i�in

df <- Hitters

df[sample(1:nrow(df),7),"Hits"] <- NA
df[sample(1:nrow(df),9),"Runs"] <- NA
df[sample(1:nrow(df),5),"RBI"] <- NA


rf_data <- missForest(df, ntree=7) # i�lem bu

#eksik de�erlerin indexleri

l <- sapply(df, function(x) which(is.na(x)))

