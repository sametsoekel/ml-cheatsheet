library(dplyr)

##data manipulation tool included

##eksik veri görselleþtirilmelidir

#groves says %20 den fazla eksik varsa sil 


#eksik veri nasýl silinir

df <- data.frame(
  v1=c(1,3,6,NA,7,1,NA,9,15),
  v2=c(7,NA,5,9,12,NA,NA,2,3),
  v3=c(NA,12,5,6,3,7,2,NA,31)
)


#BÖYLE SÝLÝNÝR 

na.omit(df) #eksik olan satýrý komple yok eder

#O deðiþkenin ortalamasýyla doldurma;

df$v1[is.na(df$v1)] <- mean(df$v1,na.rm=T)
##bunu bütün deðiþkenlere uygulamak için sapply

df <- df%>%
  sapply(function(x) ifelse(is.na(x),mean(x,na.rm = T),x))%>%
  data.frame()




#### Eksik verinin deðerlendirilmesiii ####

df <- data.frame(
  v1=c(1,3,6,NA,7,1,NA,9,15),
  v2=c(7,NA,5,9,12,NA,NA,2,3),
  v3=c(NA,12,5,6,3,7,2,NA,31)
)

sum(is.na(df)) ## kaç tane eksik deðer var ?

colSums(is.na(df)) ## sütun baþýna kaç eksik var ?

which(is.na(df)) ## konumlarýný veriyor



df[complete.cases(df),] ## eksiði olmayan gözlemleri döndürüyor

df[!complete.cases(df),] ## tam tersi


##### Eksik verinin görselleþtirilmesi #####


##ISLR kütüphanesinden hitters veri seti


df <- Hitters

colSums(is.na(df)) ## hangi kolonda ne kadar eksik ?


##Bikaç deðiþkeni eksiltiyorum gösterebilmek için


df[sample(1:nrow(df),7),"Hits"] <- NA
df[sample(1:nrow(df),9),"Runs"] <- NA
df[sample(1:nrow(df),5),"RBI"] <- NA


df[, c("Salary","Hits","Runs","RBI")]

#### Mice Kütüphanesiyle Görselleþtirme ####

md.pattern(df) ### süper bir þey



#### VIM Kütüphanesiyle Ýnceleme ####


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

#Github'dan yükledim kütüphaneleri ve CRAN'dan

t <- LittleMCAR(df)

attributes(t)

t$p.value ### bu p deðerine bakýlarak eksik deðerlerin rastgeleliliði hipotezi kontrol edilebilir



##### Eksik Verilerin Silinmesi #####
df <- Hitters

df[sample(1:nrow(df),7),"Hits"] <- NA
df[sample(1:nrow(df),9),"Runs"] <- NA
df[sample(1:nrow(df),5),"RBI"] <- NA


df <- na.omit(df) # eksik gözlem içeren hepsini siler

#bunu sadece bir deðiþken için þöyle yaparýz;

#Runs deðiþkeninin eksik olduðu gözlemleri silmek;

sapply(df[!is.na(df$Runs),], function(x) sum(is.na(x)))

# ya da kýsaca;

df <- df[!is.na(df$Runs),]

##########################################

#Bir gözlemde kaç deðiþken eksik ? Oraný nedir ?

sum(is.na(df[1,])/length(df[1,]))


g <- apply(df, 1, function(x) sum(is.na(x)) / length(x))

g <- as.vector(g)

##bunu bir deðiþken olarak nasýl ekleriz veriye


df$eksik_orani <- g


### belirli bir orandan yüksek eksiði olan verileri silme


df %>%
  filter(eksik_orani < 0.05)

######################################

######### Deðiþken Silme #########

df <- Hitters

df[sample(1:nrow(df),7),"Hits"] <- NA
df[sample(1:nrow(df),9),"Runs"] <- NA
df[sample(1:nrow(df),5),"RBI"] <- NA

#### Deðiþkenler için eksik veriyi izlemek <- funModeling lib.


status <- df_status(df) ## süper

View(status) ## süper


status[,c("variable","q_na","p_na")] ## süper


##salary deðiþkenini silmek istiyorum mesela;

df$Salary <- NULL




## Belirli bir eksiklik oranýndan fazla eksiði olan deðiþkene ulaþmak

eksik_orani <- apply(df, 2, function(x) sum(is.na(x)) / length(x))

D <- data.frame(eksik_orani)

D$degisken_ismi <- rownames(D)


## Þimdi bir koþul uygulayýp 0.02'den fazla eksiði olanlarý seçelim


D %>%
  filter(eksik_orani>0.02)




##### Deðer Atama Yöntemleri #####

df <- Hitters

df[sample(1:nrow(df),7),"Hits"] <- NA
df[sample(1:nrow(df),9),"Runs"] <- NA
df[sample(1:nrow(df),5),"RBI"] <- NA

## Ortalama ile doldurma

df$Hits[is.na(df$Hits)] <- mean(df$Hits, na.rm=T)

### Hmisc kütüphanesiyle atama yapma

summary(df$Salary) #59 boþ var diyor

df$Salary <- impute(df$Salary, mean)


  