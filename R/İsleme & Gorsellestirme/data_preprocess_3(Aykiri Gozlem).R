
####### Ayk??r?? G??zlem Yakalama #######

## Box-plot ile 

set.seed(54)
veri <- rnorm(100)

summary(veri)

## Ayk??r?? g??zlemler ekliyorum

veri <- c(veri, c(7,8,9))


boxplot(veri) ## g??rselle??tirdim

l <- boxplot.stats(veri)$out ## bu da ayk??r?? g??zlemleri ??ekiyor

## indexlerine nas??l eri??iriz ?

# b??yle;

which(veri %in% l)


##### ??ki de??i??kenin kesi??imindeki ayk??r?? g??zlem ? ####

set.seed(54)
veri <- rnorm(100)
veri <- c(veri, c(7,8,9))


set.seed(45)
veri2 <- rnorm(100)
veri2 <-c(veri2, c(9,10,11))

df <- data.frame(veri,veri2)


ggplot(df, aes(veri,veri2))+
  geom_boxplot()

l <- boxplot.stats(df$veri)$out

m <- boxplot.stats(df$veri2)$out

a <- which(df$veri %in% l) ## Veri 1 i??in ayk??r?? indexleri

b <- which(df$veri2 %in% m) ## Veri 1 i??in ayk??r?? indexleri

#### Kesi??en de??erlere nas??l eri??iriz peki ?

## Ortak ayk??r?? g??zlemlere yani

intersect(l,m)
 
## indexler de ????yle

ortak <-intersect(a,b)


points(df[ortak, ], col="red", pch="+", cex=2.5)


ortak_tum <- union(a,b)

points(df[ortak_tum, ], col="red", pch="+", cex=2.5)

### sonuncusu hem kesi??imleri hem de kendi i??lerindeki ayk??r??l??klar?? i??aretliyor