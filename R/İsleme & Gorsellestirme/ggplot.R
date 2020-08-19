library(ggplot2)

df <- iris

ggplot(df,aes(x=Sepal.Length,y=Sepal.Width))+geom_point(size=3,color="steelblue",shape="s")


df <- mpg

df$class <- factor(df$class)

glimpse(df) #dplyr dan bir özetleyici


df$disp_yeni <- factor(ifelse(df$displ>2,1,0))

#sürekli deðiþkeni kýsýtlayarak kategorik deðiþkene dönüþtürdüm

###funmodeling kütüphanesi import edildi

stat<-profiling_num(df)#sürekli deðiþken özeti

plot_num(df)#sürekli deðiþken görselleþtirme


#peki ya kategorik deðiþkenler ?

c<-freq(df) #on numara plot çiziyor

#psych kütüphanesiyle daha detaylý inceliyebiliyoz

a<-psych::describe(df)

#describe.by fonksiyonu kategorik deðiþken
#kýrýlýmýný özetler

#önce bikaç deðiþken seçelim

dum <- select(df,trans,cty,cyl,hwy)

a <- psych::describe.by(dum,dum$trans)


#Hmisc kütüphanesinde de ayný fonksiyon var

b <- Hmisc::describe(df)#bu en detaylýsý galiba


#pastecs ile de mümkün

k<-pastecs::stat.desc(df)



#d3Tree ile dataframe inceleme
d3tree(list(root= df2tree(rootname='df',
                                  struct=as.data.frame(df),
                                  toolTip ='collapse' )))

xplorerr::app_descriptive()##süper biþey


df<-diamonds

#yoðunluk grafikleri

glimpse(df) #price deðiþkeni görselleþtirme

ggplot(df,aes(price, fill=cut))+
  #geom_histogram()+
  geom_density(alpha=.3,fill="blue")+
  facet_grid(cut~.)



##ggridges ile kat. deðiþken kýrýlým ve yoðunluk inceleme


library(ggridges)

ggplot(df,aes(x=price,y=cut,fill=cut))+
  geom_density_ridges()


#interaktif histogram plotly ile
x<-rnorm(500)
x<-data.frame(x)


ggplot(x,aes(x=x))+
  geom_density()


plot_ly(x=rnorm(500),type="histogram",opacity=0.8)%>%
  add_trace(x=rnorm(200))%>%
  layout(barmode="overlay")#üst üste


####çoklu frekans

df<-diamonds
ggplot(df,aes(price,y=..density..))+
  geom_density(aes(colour=cut),binwidth=500)+
  facet_grid(cut~.)





####BOXPLOOOOOOOOOTTTTTT

ggplot(df,aes(x=cut,y=price,fill=cut))+
  geom_boxplot()
  

###violin
ggplot(df,aes(x=cut,y=price,fill=cut))+
  geom_violin()


