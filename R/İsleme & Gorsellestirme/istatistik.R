####tek örneklem T testi

olcumler <- c(17, 160, 234, 149, 145, 107, 197, 75, 201, 225, 211, 119, 
              157, 145, 127, 244, 163, 114, 145,  65, 112, 185, 202, 146,
              203, 224, 203, 114, 188, 156, 187, 154, 177, 95, 165, 50, 110, 
              216, 138, 151, 166, 135, 155, 84, 251, 173, 131, 207, 121, 120)

#hipotezimiz
#H0: M >= 170
#H1: M < 170


summary(olcumler)


##normalliði nasýl ölçeriz ?

hist(olcumler)

#normal görünüyor

## Q-Q plot yaklaþýmý (ggpubr kütüphanesi ile)

ggqqplot(olcumler) 

#nedir bu ? normallik kontrolü falan

# Shapiro-Wilks daðýlým normallik testi

shapiro.test(olcumler) #daðýlým normaldir çýktý aq


##t testi

t.test(olcumler, mu=170, alternative ="less", conf.level=0.95)

## testin sonucuna göre %95 güvenilirlikle less than 170


##alternatif bir fonksiyonla hipotez testi(inferr kütüphanesi)

df <- data.frame(olcumler)

infer_os_t_test(df,olcumler,mu=170,type=all)## en babasý bu





######NONPARAMETRÝK TEK ÖRNEKLEM TESTÝ(normallik yoksa)(DescTools ile)############


SignTest(df$olcumler,mu=170)



###########tek örneklem oran testi############
##oransal bir ifade test edilmek istendiðinde kullanýlýr#


###500 kiþi reklama týklayýp siteme gelmiþ, 40 tanesi alýþveriþ yapmýþ
#dönüþüm oraný 40/500=0.08


prop.test(x=40,n=500,p=0.05,alternative = "two.sided")



#####iki örneklem karþýlaþtýrma testi (ab tesi)####

iki_ornek_veri <- data.frame(
  
  A = c(30,27,21,27,29,30,20,20,27,32,35,22,24,23,25,27,23,27,23,
        25,21,18,24,26,33,26,27,28,19,25),
  
  B = c(37,39,31,31,34,38,30,36,29,28,38,28,37,37,30,32,31,31,27,
        32,33,33,33,31,32,33,26,32,33,29)
)


##hipotez

#h0: m1 = m2
#h1: m1 != m2

##funmodeling gerekiyor

s <- profiling_num(iki_ornek_veri)


ggplot(iki_ornek_veri,aes(A,B))+
  geom_boxplot()

#iki ayrý ölçümün tek bir dataframe de olmamasý gerekiyor


A <- data.frame(degerler= iki_ornek_veri$A, sinif="A")
B <- data.frame(degerler= iki_ornek_veri$B, sinif="B")


AB <- rbind(A,B)

ggplot(AB,aes(sinif,degerler, fill=sinif))+
  geom_boxplot()

## varsayým kontrolleri

#normalliðin incelenmesi

ggplot(AB,aes(degerler,fill=sinif))+
  geom_histogram(aes(y=..density..),color="black",alpha=.5,binwidth = 5)+
  facet_grid(sinif~. )+#bunu unutma, ayýrmak için
  geom_density(alpha=.3)


# numerik test

apply(iki_ornek_veri,2,shapiro.test)


##varyans homojenliðinin incelenmesi (car library)

leveneTest(AB$degerler ~ AB$sinif, center=mean) 

## korelasyon

df <- mtcars

m <- cor(df,use="complete.obs")## sadece varolan gözlemler üzerinden korelasyon hesaplar 


#Hmisc library daha detaylý (p-value içeren)

rcorr(as.matrix(df))


#### daha geliþmiþi PerformanceAnalytics library ile

df <- mtcars[,c(1,3,4,5,6,7)]

chart.Correlation(df,histogram=T,pch=19)###süper




