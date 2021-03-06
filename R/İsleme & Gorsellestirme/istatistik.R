####tek �rneklem T testi

olcumler <- c(17, 160, 234, 149, 145, 107, 197, 75, 201, 225, 211, 119, 
              157, 145, 127, 244, 163, 114, 145,  65, 112, 185, 202, 146,
              203, 224, 203, 114, 188, 156, 187, 154, 177, 95, 165, 50, 110, 
              216, 138, 151, 166, 135, 155, 84, 251, 173, 131, 207, 121, 120)

#hipotezimiz
#H0: M >= 170
#H1: M < 170


summary(olcumler)


##normalli�i nas�l �l�eriz ?

hist(olcumler)

#normal g�r�n�yor

## Q-Q plot yakla��m� (ggpubr k�t�phanesi ile)

ggqqplot(olcumler) 

#nedir bu ? normallik kontrol� falan

# Shapiro-Wilks da��l�m normallik testi

shapiro.test(olcumler) #da��l�m normaldir ��kt� aq


##t testi

t.test(olcumler, mu=170, alternative ="less", conf.level=0.95)

## testin sonucuna g�re %95 g�venilirlikle less than 170


##alternatif bir fonksiyonla hipotez testi(inferr k�t�phanesi)

df <- data.frame(olcumler)

infer_os_t_test(df,olcumler,mu=170,type=all)## en babas� bu





######NONPARAMETR�K TEK �RNEKLEM TEST�(normallik yoksa)(DescTools ile)############


SignTest(df$olcumler,mu=170)



###########tek �rneklem oran testi############
##oransal bir ifade test edilmek istendi�inde kullan�l�r#


###500 ki�i reklama t�klay�p siteme gelmi�, 40 tanesi al��veri� yapm��
#d�n���m oran� 40/500=0.08


prop.test(x=40,n=500,p=0.05,alternative = "two.sided")



#####iki �rneklem kar��la�t�rma testi (ab tesi)####

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

#iki ayr� �l��m�n tek bir dataframe de olmamas� gerekiyor


A <- data.frame(degerler= iki_ornek_veri$A, sinif="A")
B <- data.frame(degerler= iki_ornek_veri$B, sinif="B")


AB <- rbind(A,B)

ggplot(AB,aes(sinif,degerler, fill=sinif))+
  geom_boxplot()

## varsay�m kontrolleri

#normalli�in incelenmesi

ggplot(AB,aes(degerler,fill=sinif))+
  geom_histogram(aes(y=..density..),color="black",alpha=.5,binwidth = 5)+
  facet_grid(sinif~. )+#bunu unutma, ay�rmak i�in
  geom_density(alpha=.3)


# numerik test

apply(iki_ornek_veri,2,shapiro.test)


##varyans homojenli�inin incelenmesi (car library)

leveneTest(AB$degerler ~ AB$sinif, center=mean) 

## korelasyon

df <- mtcars

m <- cor(df,use="complete.obs")## sadece varolan g�zlemler �zerinden korelasyon hesaplar 


#Hmisc library daha detayl� (p-value i�eren)

rcorr(as.matrix(df))


#### daha geli�mi�i PerformanceAnalytics library ile

df <- mtcars[,c(1,3,4,5,6,7)]

chart.Correlation(df,histogram=T,pch=19)###s�per




