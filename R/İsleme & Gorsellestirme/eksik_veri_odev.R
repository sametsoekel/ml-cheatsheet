odev <- mtcars

odev$mpg[sample(1:nrow(odev),3)] <- NA

odev$qsec[sample(1:nrow(odev),2)] <- NA

odev[!complete.cases(odev),]

# Rassallýðýn testi için BaylorEdPsych, LittleMCAR

t <- LittleMCAR(odev)

t$p.value #Rassal deðil galiba

# Mice, md.pattern ile görselleþtirme

md.pattern(odev[,c("qsec","mpg")])

#### eksik gözlemlerin indexleri

l <- sapply(odev,function(x) which(is.na(x)))

mtcars$mpg[l$mpg]-odev_dolu$mpg[l$mpg]

odev_dolu <- knnImputation(odev,k=12)

odev_dolu$mpg[l$mpg]