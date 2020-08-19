
##### Train-Test Ayrımı #####

df <- ISLR::Hitters

dff <- na.omit(df)

plot(df[,c("AtBat","Hits","Runs","Salary")])

ggplot(df,aes(AtBat,Hits))+
  geom_point()