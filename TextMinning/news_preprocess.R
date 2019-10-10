# 将新闻拆解为单句，留下日期，加上序号
library("jiebaR")
library("stringr")
library("dplyr")
alldat <- readRDS("newsdata.rds")

get_sent <- function(dat1){
  s2 <- data.frame(date=dat1[1],
                   news=str_trim(unlist(str_split(dat1[3],"。"))))
  s2$num <- seq(1:nrow(s2))
  return(s2)
}

newsdat <- data.frame(date="",news="",num="")[-1,]

for (i in 1:900){
  temp <- alldat[((i-1)*500+1):(i*500),]
  x <- do.call("rbind", apply(temp, 1, get_sent))
  print(i)
  saveRDS(x, paste0("newsdata/newsdat",i,".rds"))
}

temp <- alldat[450001:450036,]
x <- do.call("rbind", apply(temp, 1, get_sent))
saveRDS(x, paste0("newsdata/newsdat901.rds"))





