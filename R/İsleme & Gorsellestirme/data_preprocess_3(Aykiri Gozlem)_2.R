
#### Ayk�r� G�zlemlerden Kurtulma ####

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

### Ayk�r� g�zlemlerin indexleri

ortak_tum <- union(a,b) ##B�t�n ayk�r� g�zlemler

df[-ortak_tum,] #bu komut ayk�r� g�zlemleri df'den siliyor


##### Ortalama ile Doldurma #####


a <- which(df$x %in% l) 

b <- which(df$y %in% m)


df[a,]$x #ayk�r� x g�zlemleri

df[b,]$y #ayk�r� y g�zlemleri

## dolduruyorum


df[b,]$y <- mean(df$y)

## doldurdum




##### Bask�lama #####


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




summary(df$x)[5] #3. �eyrek de�eri

fivenum(df$x)[4] ## bu da olur

fivenum(df$x)[2] ## bu da 1. �eyrek

df[a, ]$x <- ifelse(df[a, ]$x>0,fivenum(df$x)[4],fivenum(df$x)[2]) 
## s�f�rdan b�y�k olanlara 3. k���k olanlara 1. �eyrek de�erini bast�m

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

### Ayk�r� g�zlemlerin indexleri

ortak_tum <- union(a,b) ##B�t�n ayk�r� g�zlemler


df[ortak_tum,] ### �nce ayk�r� g�zlemleri siliyoruz

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
