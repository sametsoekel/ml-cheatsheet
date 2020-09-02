
download.file("http://www.gutenberg.org/cache/epub/1661/pg1661.txt",
              destfile = "sherlock.txt")


setwd("C:/Users/user/Desktop/R/Text Mining")


sherlock <- readLines("sherlock.txt")

## save(sherlock, file = "sherlock.rda")

#rm(sherlock)

#load(file ="sherlock.rda")

head(sherlock)
length(sherlock)
tail(sherlock)



