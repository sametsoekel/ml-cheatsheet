library(tidyverse)
library(cluster)
library(factoextra)



df <- USArrests 

# 50 Abd eyaletine ait suç istatistikleri, eyaletleri
## iğrençlik metriklerine göre kümeleyeceğim


df <- scale(df) ## Ölçekleme



#### Uzaklık Matrisi Ve Görseli #######


distance <- get_dist(df)
fviz_dist(distance)




##### Kmeans modeli ######


k2 <- kmeans(df, centers=2, nstart = 25)

str(k2)




######### Kümelerin Görselleştirilmesi ########


fviz_cluster(k2, data=df)



### Scatter plot ile görselleştirme

df %>% as_tibble() %>% 
  mutate(kumeler = k2$cluster,
         eyaletler = rownames(USArrests)) %>% 
  ggplot(aes(UrbanPop,Murder,color=factor(kumeler), label = eyaletler))+
  geom_text()


#### Murder ve UrbanPop değişkenleri arasında eyaletler nasıl dağılıyor ?


##### Standartlaşmamış haliyle bakalım



df <- USArrests 


df %>% as_tibble() %>% 
  mutate(kumeler = k2$cluster,
         eyaletler = rownames(USArrests)) %>% 
  ggplot(aes(UrbanPop,Murder,color=factor(kumeler), label = eyaletler))+
  geom_text()











########### OPTİMUM KÜME SAYISINI BULMAK ###########


df <- scale(df)

#### Farklı k değerleri deneme / gözlemleme #####

k2

k3 <- kmeans(df, centers=3, nstart = 25)

k4 <- kmeans(df, centers=4, nstart = 38)

k5 <- kmeans(df, centers=5, nstart = 25)



p1 <- fviz_cluster(k2, geom="point",data=df)+ggtitle("k=2")

p2 <- fviz_cluster(k3, geom="point",data=df)+ggtitle("k=3")

p3 <- fviz_cluster(k4, geom="point",data=df)+ggtitle("k=4")

p4 <- fviz_cluster(k5, geom="point",data=df)+ggtitle("k=5")



### gridExtra kütüphanesiyle tek bir plot

grid.arrange(p1,p2,p3,p4,nrow=2) ## süper





###### Elbow yöntemi #########

set.seed(42)

fviz_nbclust(df,kmeans,method="wss") ### Küme sayısına göre hata grafiği


### ben grafikten 4 küme optimaldir yorumunu çıkardım....







######## Average Silhouette Yöntemi #######


## küme içi kalite skoru hesaplayarak optimal küme sayısı belirlemeye çalışır


fviz_nbclust(df,kmeans,method="silhouette")

## Bu metrik 2 kümenin optimal olduğunu söylüyor









##### GAP İstatistiği ####### most suggested


set.seed(42)

gap_stat <- clusGap(df,FUN=kmeans, nstart=25, K.max = 10, B = 50)


fviz_gap_stat(gap_stat) ### 4 diyor







#### final kmeans modeli ######

set.seed(42)

final <- kmeans(df,centers=4,nstart=25)

final$cluster ## lingo lingo kümeler


fviz_cluster(final, geom="text",data=df)+ggtitle("Final Kümelemesi")



View(USArrests)
####### KÜMELERE GÖRE BETİMSEL İSTATİSTİKLER ######

USArrests %>% 
  mutate(Cluster = final$cluster) %>% 
  group_by(Cluster) %>% 
  summarise_all("mean") 

#### Her bir kümenin değişkenlerinin ortalamaları


  

