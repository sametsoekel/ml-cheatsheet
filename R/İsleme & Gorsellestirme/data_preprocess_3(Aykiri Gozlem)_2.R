
#### Aykýrý Gözlemlerden Kurtulma ####

set.seed(54)
x <- rnorm(100)
x <- c(x, c(7,8,9))


set.seed(45)
y <- rnorm(100)
y <-c(y, c(9,10,11))

df <- data.frame(x,y)

l <- boxplot.stats(df$x)$out

m <- boxplot.stats(df$y)$out

a <- which(df$x %in% l) 

b <- which(df$y %in% m)

### Aykýrý gözlemlerin indexleri

ortak_tum <- union(a,b) ##Bütün aykýrý gözlemler

df[-ortak_tum,] #bu komut aykýrý gözlemleri df'den siliyor


##### Ortalama ile Doldurma #####


a <- which(df$x %in% l) 

b <- which(df$y %in% m)


df[a,]$x #aykýrý x gözlemleri

df[b,]$y #aykýrý y gözlemleri

## dolduruyorum


df[b,]$y <- mean(df$y)

## doldurdum




##### Baskýlama #####


set.seed(54)
x <- rnorm(100)
x <- c(x, c(7,8,9))


set.seed(45)
y <- rnorm(100)
y <-c(y, c(9,10,11))

df <- data.frame(x,y)

l <- boxplot.stats(df$x)$out

m <- boxplot.stats(df$y)$out

a <- which(df$x %in% l) 

b <- which(df$y %in% m)




summary(df$x)[5] #3. çeyrek deðeri

fivenum(df$x)[4] ## bu da olur

fivenum(df$x)[2] ## bu da 1. çeyrek

df[a, ]$x <- ifelse(df[a, ]$x>0,fivenum(df$x)[4],fivenum(df$x)[2]) 
## sýfýrdan büyük olanlara 3. küçük olanlara 1. çeyrek deðerini bastým

boxplot(df$x)

ggplot(df,aes(x))+
  geom_density()


###### Tahmin ile Doldurma #####

set.seed(54)
x <- rnorm(100)
x <- c(x, c(7,8,9))


set.seed(45)
y <- rnorm(100)
y <-c(y, c(9,10,11))

df <- data.frame(x,y)

l <- boxplot.stats(df$x)$out

m <- boxplot.stats(df$y)$out

a <- which(df$x %in% l) 

b <- which(df$y %in% m)

### Aykýrý gözlemlerin indexleri

ortak_tum <- union(a,b) ##Bütün aykýrý gözlemler


df[ortak_tum,] ### Önce aykýrý gözlemleri siliyoruz

df[a,]$x <- NA
df[b,]$y <- NA

#### DMwR, knn ile dolduruyorum

df <- knnImputation(df,k=5)

##########################################




a <- rnorm(200)

outliers <- boxplot.stats(a)$out

index_outliers <- which(a %in% outliers)

a[index_outliers] <- ifelse(a[index_outliers]>0,fivenum(a)[4],fivenum(a)[2])

plot(density(a))
