library("jiebaR")
library("stringr")
library("dplyr")
gcinfo(verbose = FALSE)
sdat <- read.csv("mydic/stocklist",stringsAsFactors = FALSE)
sdat$Stkcd <- formatC(sdat$Stkcd, width=6, flag="0")

newsdata <- readRDS("news/news5.rds")[,c(1,2)]

jnnews <- data.frame(date="", news="", Stkcd="", Stknme="")[-1, ]
for(i in 1:3){
  num1 <- grepl(sdat$Stkcd[i],newsdata$news) | grepl(sdat$Stknme[i],newsdata$news)
  if (sum(num1)>0){
    temp <- data.frame(newsdata[num1,], sdat[i,])
    temp <- temp[!duplicated(temp$news),]
    jnnews <- rbind(jnnews,temp)
    rm(temp, num1)
    gc(reset = TRUE)
  }
  print(paste0(i,": ", Sys.time()))
}


