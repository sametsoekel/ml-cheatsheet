###korelasyonlarýn incelenmesi
##scatter plot
df <- iris
glimpse(df)

ggplot(df,aes(x=Sepal.Width,y=Sepal.Length,color=Species,shape=Species))+
  geom_point(size=3,
             alpha=0.6)



ggplot(df,aes(x=Sepal.Width,y=Sepal.Length,color=Petal.Length,size=Petal.Length))+
  geom_point(size=3,
             alpha=0.6)


#scatterplot birimleri grafige eklemek

glimpse(df)

df <- mtcars

ggplot(df,aes(wt,mpg, fill=cyl))+
  geom_point()+
  geom_label(label=rownames(df),
            nudge_x = 0.25,
            nudge_y = 0.25)+
  geom_smooth(method=lm, se=F)



#marjinlere daðýlým eklemek ggExtra

g <- ggplot(df,aes(wt,mpg, fill=cyl))+
  geom_point()+
  geom_label(label=rownames(df),
             nudge_x = 0.25,
             nudge_y = 0.25)+
  geom_smooth(method=lm, se=F)


ggMarginal(g, type="histogram")



#ýsý haritasý ile korelasyon yakalama


df <- as.matrix(mtcars)

heatmap(df,Colv=NA, Rowv=NA,scale="column")




##korelasyon matrisleri

df <- mtcars[,c(1,3:6)]

gg <- cor(df) #korelasyon deðerleri matris olarak

cor.test(df$mpg,df$disp) #korelasyonun anlamlýlýðý

plot(df) #korelasyon grafikleri


## daha sexy olaný

ggcorr(df)

ggpairs(df)




####Zaman serisi görselleþtirme

df <- economics

ggplot(df,aes(date,pop))+
  geom_line(aes(size=unemploy/pop))

ggplot(df,aes(date,psavert))+
  geom_line()+
  stat_smooth()

##########dairesel barplot############

veri <- data.frame(id = seq(1,60),
                   ogrenciler = paste("Ogrenci",seq(1,60),sep="_"),
                   notlar = sample(seq(10,100),60,replace=T))


ggplot(veri, aes(x=factor(id),y=notlar))+
  geom_bar(stat="identity")+
  ylim(-100,120)+
  theme(
    axis.text=element_blank(),
    axis.title=element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4),"cm")
  )+
  coord_polar(start=0)

#Süper önemli temaaaa yukarda


##etiketler de ekleyelim

label_data <- veri
sutun_sayisi <- nrow(label_data)
aci <- 90 - 360*(label_data$id-0.5)/sutun_sayisi
label_data$hjust <- ifelse(aci< -90, 1 ,0)
label_data$aci <- ifelse(aci< -90, aci+180,aci)

ggplot(veri, aes(x=factor(id),y=notlar))+
  geom_bar(stat="identity")+
  ylim(-100,120)+
  theme(
    axis.text=element_blank(),
    axis.title=element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4),"cm")
  )+
  coord_polar(start=0)+
  geom_text(data=label_data,aes(x=id,
                                y=notlar+10,
                                label=ogrenciler,
                                hjust=hjust),color="black",
            fontface="bold", alpha=0.6,size=2.5,
            angle=label_data$aci, inherit.aes=FALSE)


##########dairesel barplot finito############



########treemap kütüphanesiyle treemap##########
df <- data.frame(
  gruplar=c("grup_1","grup_2","grup_3"),
  degerler=c(10,90,60)
)

treemap(df,index="gruplar",vSize="degerler",type="index")

#alt gruplar da þöyle#

df2 <- data.frame(
  gruplar=c(rep("grup_1",4),
            rep("grup_2",2),
            rep("grup_3",3)),
  alt_gruplar = paste("alt_grup",c(1,2,3,4,1,2,1,2,3),sep="_" ),
  degerler=c(13,5,22,12,11,4,2,5,6)
)


treemap(df2,index=c("gruplar","alt_gruplar"),
        vSize="degerler",
        type="index",
        fontsize.labels = c(20,13),
        fontcolor.labels = c("black","white"),
        bg.labels=c("transparent"))
