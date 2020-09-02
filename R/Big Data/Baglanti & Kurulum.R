install.packages("sparklyr")
library(sparklyr)
library(tidyverse)
library(nycflights13)
library(DBI)




# spark_install() bir kereye mahsus

sc <- spark_connect(master ="local") ## bağlantı kurmak



#### Verinin spark'a taşınması #####

src_tbls(sc) # spark'da veri varmı diye kontrol


f_tbl <- copy_to(sc, nycflights13::flights, 
                 "flights_spark",overwrite = T) ## taşıma işlemi


## bu veri yapısı dplyr ile kullanılabiliyor



f_tbl %>% filter(dep_time==100) ## bir örnek
