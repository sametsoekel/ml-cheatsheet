df <- USArrests
df <- scale(df)


### diana ###

hc4 <- diana(df)

hc4$dc ## bölümlenme katsayısı


pltree(hc4)




####### Dendogramları ayarlamak ###########



den <- hclust(d, method ="ward.D2")

plot(den) ## bu dendogramı bölelim şimdi




alt_grup <- cutree(den, k=4) ### 4 sınıfa böl dedim



table(alt_grup) ## her bir kümedeki bileşen sayısı


alt_grup ## hangi gözlem hangi grupta ?




### VERİ SETİNDE KÜMELERİ GÖSTERMEK ######

USArrests %>% 
  mutate(cluster=alt_grup) %>% 
  filter(cluster == 1)  #### Sadece 1. kümedeki gözlemleri çekiyor



rect.hclust(den, k=4,border=5:8) #### Kümeleri görselleştirmek











