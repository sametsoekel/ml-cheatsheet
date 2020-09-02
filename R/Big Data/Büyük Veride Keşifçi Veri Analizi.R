

## baglanti scriptinde veriyi spark'a aktarmıştım


f_tbl %>% filter(dep_delay<100) %>% 
  select(year,month,dep_time)


f_tbl %>% 
  summarise(ort=mean(dep_delay,na.rm = T),
            sd=sd(dep_delay),
            varyans=var(dep_delay))


f <- f_tbl %>% group_by(carrier) %>% 
  summarise(n=n()) %>% 
  collect()


## collect fonksiyonu R çevresinde fiziki çalıştırabilmek için


f$carrier # mesela fiziki olarak çevreye çektikten sonra böyle erişebiliyorum





