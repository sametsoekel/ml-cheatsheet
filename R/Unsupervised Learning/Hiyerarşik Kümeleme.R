


#### Birleştirici hiyerarşik kümeleme #######

df <- USArrests

apply(df,2,var) # Tüm değişkenlerin varyansları, standartlaştırmam gerekiyor


df <- scale(df)

d <- dist(df,method = "euclidean") ## Tüm uzaklıklar




##### kümeleme ####

hc1 <- hclust(d, method = "complete")

plot(hc1) ## Süper bir dendogram





###### birleştirici metodların karşılaştırılması #####


## bu fonksiyon aynı zamanda birleştiricilik katsayısı diye bir
## metrik de sağlıyor. Bu metrik ile küme yapısının gücünü ölçebiliyoruz.


hc2 <- agnes(df, method = "complete")

hc2$ac ## the katsayı, bunu kullanarak optimal kümelemeyi bulacağız







## bir nevi tuning işlemi gerçekleştiriyorum


m <- c("average","single","complete","ward")

names(m) <- c("average","single","complete","ward")





ac <- function(x){
  
  agnes(df, method =x)$ac
  
}



sapply(m, ac) ## ward en güçlüsü çıktı




hc3 <-  agnes(df, method = "ward")


pltree(hc3)











