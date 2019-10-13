# *******************************************************************
# 程式說明: 交易日轉事件日, 產生事件日資料
# *******************************************************************
# 輸入資料: 
#  1. event data 
#  2. stock historical close prices 
#  3. market index (close price)
# 輸出資料:
#  a list data 

# *******************************************************************
#  Packages
# *******************************************************************
library(eventstudies)
library(dplyr)
library(tidyr)

# *******************************************************************
# 匯入所需要的資料
# *******************************************************************
# Input 1: event data (only two variables)
edat <- read.csv("edata1t.csv", colClasses=c("character","Date"))
names(edat) <- c("name","when")
# 去除重複的event, 代碼前加上"X"
edat <- edat[!duplicated(edat),] %>%
  mutate(name=paste0("X",name))
# Input 2: all stock trading data, 代碼前加上"X"
sdat <- readRDS("Cstock.rds") %>%
  mutate(Stkcd=paste0("X",Stkcd))
# Input 3: market data (000001)
mdat <- readRDS("Mindex.rds")

# *******************************************************************
#  選擇事件的時間範圍 (預設可包含全部事件)
# ******************************************************************* 
# 出現新聞日期
nowdate <- as.Date("2017-06-30")
# 多少天前事件納入考慮
eventinerval <- 4000
useevent <- edat %>%
  filter(when<=nowdate, when>=nowdate-eventinerval)

# *******************************************************************
#  調整stock data
# ******************************************************************* 
# 刪除沒發生事件的股票
sprice <- sdat %>%
  filter(Stkcd %in% useevent$name) %>%
  select(Trddt,Stkcd,Clsprc)

# 讓欄位名稱變成股票代碼 => 符合phys2eventtime()的要求
sprice <- spread(sprice,Stkcd,Clsprc)
# 處理NA值, 若na，以之後第一筆非NA的數值補上
# 注意：每檔股票前後仍可能有NA值
sprice <- na.locf(sprice)
# 轉換為xts
sprice <- xts(sprice[,-1], as.Date(sprice$Trddt))

# *******************************************************************
#  調整 market data
# ******************************************************************* 
mprice <- mdat %>%
  select(Trddt,Clsindex)

mprice <- xts(mprice[,-1], as.Date(mprice$Trddt))
names(mprice) <- "mindex"

sprice <- merge.xts(sprice,mprice,join="inner")
mevent <- useevent %>%
  mutate(name="mindex")

# *******************************************************************
#  
# ******************************************************************* 
# 估計期長度 (使用者自訂)
esday=60
# 事件期長度 (使用者自訂)
evday=120

es <- phys2eventtime(z = sprice, events = useevent, width = 20)
es.w <- window(es$z.e,start=-(esday+evday), end=evday)
# 選擇有用的event
useevent <- useevent[es$outcomes=="success",]
mevent <- mevent[es$outcomes=="success",]

# 產生市場價格事件價格 (事件日與es.w相同)
mes <- phys2eventtime(z = sprice$mindex, events = mevent)
mes.w <- window(mes$z.e,start=-(esday+evday), end=evday)

#eventdata <- list(event=useevent, stock=es.w, market=mes.w, alldate=mdat$Trddt)



eventdata <- list(event=useevent, 
                  stock=es.w, 
                  market=mes.w, 
                  alldate=mdat$Trddt)

saveRDS(eventdata,"Event0002.rds")
