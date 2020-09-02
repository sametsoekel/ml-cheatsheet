
f <- f_tbl %>% group_by(carrier) %>% 
  summarise(n=n()) %>% 
  collect()


ggplot(f,aes(carrier,n))+
  geom_bar(stat="identity")


## gibi gibi