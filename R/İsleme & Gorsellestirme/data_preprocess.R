library(dplyr)

##data manipulation tool included

##eksik veri g�rselle�tirilmelidir

#groves says %20 den fazla eksik varsa sil 


#eksik veri nas�l silinir

df <- data.frame(
  v1=c(1,3,6,NA,7,1,NA,9,15),
  v2=c(7,NA,5,9,12,NA,NA,2,3),
  v3=c(NA,12,5,6,3,7,2,NA,31)
)


#B�YLE S�L�N�R 

na.omit(df) #eksik olan sat�r� komple yok eder

#O de�i�kenin ortalamas�yla doldurma;

df$v1[is.na(df$v1)] <- mean(df$v1,na.rm=T)
##bunu b�t�n de�i�kenlere uygulamak i�in sapply

df <- df%>%
  sapply(function(x) ifelse(is.na(x),mean(x,na.rm = T),x))%>%
  data.frame()




#### Eksik verinin de�erlendirilmesiii ####

df <- data.frame(
  v1=c(1,3,6,NA,7,1,NA,9,15),
  v2=c(7,NA,5,9,12,NA,NA,2,3),
  v3=c(NA,12,5,6,3,7,2,NA,31)
)

sum(is.na(df)) ## ka� tane eksik de�er var ?

colSums(is.na(df)) ## s�tun ba��na ka� eksik var ?

which(is.na(df)) ## konumlar�n� veriyor



df[complete.cases(df),] ## eksi�i olmayan g�zlemleri d�nd�r�yor

df[!complete.cases(df),] ## tam tersi


##### Eksik verinin g�rselle�tirilmesi #####


##ISLR k�t�phanesinden hitters veri seti


df <- Hitters

colSums(is.na(df)) ## hangi kolonda ne kadar eksik ?


##Bika� de�i�keni eksiltiyorum g�sterebilmek i�in


df[sample(1:nrow(df),7),"Hits"] <- NA
df[sample(1:nrow(df),9),"Runs"] <- NA
df[sample(1:nrow(df),5),"RBI"] <- NA


df[, c("Salary","Hits","Runs","RBI")]

#### Mice K�t�phanesiyle G�rselle�tirme ####

md.pattern(df) ### s�per bir �ey



#### VIM K�t�phanesiyle �nceleme ####


aggr_plot <- aggr(df, col=c("navyblue","red"),
                  numbers=T,
                  sortVars=T,
                  labels=names(df),
                  cex.axis=.7,
                  gap=3,
                  ylab=c("Eksik Degerlerin Oransal Gosterimi",
                         "Eksikligin Veri Seti Icindeki Yapisi"))



#### Eksik Veri Rassalligin Testi ####

##Required Libraries: BaylorEdPsych, mvnmle

#Github'dan y�kledim k�t�phaneleri ve CRAN'dan

t <- LittleMCAR(df)

attributes(t)

t$p.value ### bu p de�erine bak�larak eksik de�erlerin rastgelelili�i hipotezi kontrol edilebilir



##### Eksik Verilerin Silinmesi #####
df <- Hitters

df[sample(1:nrow(df),7),"Hits"] <- NA
df[sample(1:nrow(df),9),"Runs"] <- NA
df[sample(1:nrow(df),5),"RBI"] <- NA


df <- na.omit(df) # eksik g�zlem i�eren hepsini siler

#bunu sadece bir de�i�ken i�in ��yle yapar�z;

#Runs de�i�keninin eksik oldu�u g�zlemleri silmek;

sapply(df[!is.na(df$Runs),], function(x) sum(is.na(x)))

# ya da k�saca;

df <- df[!is.na(df$Runs),]

##########################################

#Bir g�zlemde ka� de�i�ken eksik ? Oran� nedir ?

sum(is.na(df[1,])/length(df[1,]))


g <- apply(df, 1, function(x) sum(is.na(x)) / length(x))

g <- as.vector(g)

##bunu bir de�i�ken olarak nas�l ekleriz veriye


df$eksik_orani <- g


### belirli bir orandan y�ksek eksi�i olan verileri silme


df %>%
  filter(eksik_orani < 0.05)

######################################

######### De�i�ken Silme #########

df <- Hitters

df[sample(1:nrow(df),7),"Hits"] <- NA
df[sample(1:nrow(df),9),"Runs"] <- NA
df[sample(1:nrow(df),5),"RBI"] <- NA

#### De�i�kenler i�in eksik veriyi izlemek <- funModeling lib.


status <- df_status(df) ## s�per

View(status) ## s�per


status[,c("variable","q_na","p_na")] ## s�per


##salary de�i�kenini silmek istiyorum mesela;

df$Salary <- NULL




## Belirli bir eksiklik oran�ndan fazla eksi�i olan de�i�kene ula�mak

eksik_orani <- apply(df, 2, function(x) sum(is.na(x)) / length(x))

D <- data.frame(eksik_orani)

D$degisken_ismi <- rownames(D)


## �imdi bir ko�ul uygulay�p 0.02'den fazla eksi�i olanlar� se�elim


D %>%
  filter(eksik_orani>0.02)




##### De�er Atama Y�ntemleri #####

df <- Hitters

df[sample(1:nrow(df),7),"Hits"] <- NA
df[sample(1:nrow(df),9),"Runs"] <- NA
df[sample(1:nrow(df),5),"RBI"] <- NA

## Ortalama ile doldurma

df$Hits[is.na(df$Hits)] <- mean(df$Hits, na.rm=T)

### Hmisc k�t�phanesiyle atama yapma

summary(df$Salary) #59 bo� var diyor

df$Salary <- impute(df$Salary, mean)


  