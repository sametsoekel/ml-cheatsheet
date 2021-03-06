library(ggplot2)

df <- iris

ggplot(df,aes(x=Sepal.Length,y=Sepal.Width))+geom_point(size=3,color="steelblue",shape="s")


df <- mpg

df$class <- factor(df$class)

glimpse(df) #dplyr dan bir �zetleyici


df$disp_yeni <- factor(ifelse(df$displ>2,1,0))

#s�rekli de�i�keni k�s�tlayarak kategorik de�i�kene d�n��t�rd�m

###funmodeling k�t�phanesi import edildi

stat<-profiling_num(df)#s�rekli de�i�ken �zeti

plot_num(df)#s�rekli de�i�ken g�rselle�tirme


#peki ya kategorik de�i�kenler ?

c<-freq(df) #on numara plot �iziyor

#psych k�t�phanesiyle daha detayl� inceliyebiliyoz

a<-psych::describe(df)

#describe.by fonksiyonu kategorik de�i�ken
#k�r�l�m�n� �zetler

#�nce bika� de�i�ken se�elim

dum <- select(df,trans,cty,cyl,hwy)

a <- psych::describe.by(dum,dum$trans)


#Hmisc k�t�phanesinde de ayn� fonksiyon var

b <- Hmisc::describe(df)#bu en detayl�s� galiba


#pastecs ile de m�mk�n

k<-pastecs::stat.desc(df)



#d3Tree ile dataframe inceleme
d3tree(list(root= df2tree(rootname='df',
                                  struct=as.data.frame(df),
                                  toolTip ='collapse' )))

xplorerr::app_descriptive()##s�per bi�ey


df<-diamonds

#yo�unluk grafikleri

glimpse(df) #price de�i�keni g�rselle�tirme

ggplot(df,aes(price, fill=cut))+
  #geom_histogram()+
  geom_density(alpha=.3,fill="blue")+
  facet_grid(cut~.)



##ggridges ile kat. de�i�ken k�r�l�m ve yo�unluk inceleme


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
  layout(barmode="overlay")#�st �ste


####�oklu frekans

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


