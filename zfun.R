# version: 20171229

library(zoo)
library(ggplot2)
library(dplyr)
# 計算某事件區間n1到n2的累積報酬率 (扣除市場報酬)
CAAR <- function(mydat, nowdate, startdate, n1, n2){
  edate <- which(mydat$event$when<nowdate-n2 &
                   mydat$event$when>=startdate)

  event <- mydat$event[edate,]
  es.w <- mydat$stock[,edate]
  mk.w <- mydat$market[,edate]

  s0 <- matrix(1,n2-n1)%*%coredata(es.w[as.character(n1)])
  m0 <- matrix(1,n2-n1)%*%coredata(mk.w[as.character(n1)])

  sn <- coredata(es.w[as.character((n1+1):n2)])
  mn <- coredata(mk.w[as.character((n1+1):n2)])

  sret <- (sn-s0)/s0
  mret <- (mn-m0)/m0
  aret <- sret-mret

  # adj_cret: 各期累積異常報酬
  adj_cret <- rowMeans(aret, na.rm = TRUE)
  # raw_cret: 各期未調整累積報酬
  raw_cret <- rowMeans(sret, na.rm = TRUE)

  # adj_winprob: 各期累積異常報酬勝率超越市場
  winmatrix <- aret
  winmatrix[winmatrix>=0] <- 1
  winmatrix[winmatrix<0] <- 0
  adj_winprob <- rowSums(winmatrix)/ncol(winmatrix)

  # raw_winprob: 各期累積報酬勝率大於0
  winmatrix <- sret
  winmatrix[winmatrix>=0] <- 1
  winmatrix[winmatrix<0] <- 0
  raw_winprob <- rowSums(winmatrix)/ncol(winmatrix)

  CAAR <- list(adj_cret=adj_cret,
               raw_cret=raw_cret,
               adj_winprob=adj_winprob,
               raw_winprob=raw_winprob,
               event=event)
}

# Ef_enum() 事件發生數
Ef_enum <- function(mydat, nowdate=Sys.Date(), startdate=as.Date("2005-01-01"), n1=0, n2=60){
  edate <- which(mydat$event$when<nowdate-n2 &
                   mydat$event$when>=startdate)
  event <- mydat$event[edate,]
  Ef_enum <- nrow(event)
}

# Ef_car() 期末(n日)累積異常報酬率
Ef_car <- function(mydat, nowdate=Sys.Date(), startdate=as.Date("2005-01-01"), n1=0, n2=60){
  edate <- which(mydat$event$when<nowdate-n2 &
                   mydat$event$when>=startdate)
  event <- mydat$event[edate,]
  es.w <- mydat$stock[,edate]
  mk.w <- mydat$market[,edate]

  s0 <- coredata(es.w[as.character(n1)])
  m0 <- coredata(mk.w[as.character(n1)])

  sn <- coredata(es.w[as.character(n2)])
  mn <- coredata(mk.w[as.character(n2)])

  sret <- (sn-s0)/s0
  #sret <- log(sn)-log(s0)
  mret <- (mn-m0)/m0
  #mret <- log(mn)-log(m0)
  aret <- sret-mret

  # adj_cret: 各期累積異常報酬
  Ef_car <- mean(aret, na.rm = TRUE)
}

# Ef_scr() 期末(n日)累積報酬率
Ef_scr <- function(mydat, nowdate=Sys.Date(), startdate=as.Date("2005-01-01"), n1=0, n2=60){
  edate <- which(mydat$event$when<nowdate-n2 &
                   mydat$event$when>=startdate)
  event <- mydat$event[edate,]
  es.w <- mydat$stock[,edate]
  s0 <- coredata(es.w[as.character(n1)])
  sn <- coredata(es.w[as.character(n2)])
  sret <- (sn-s0)/s0

  # adj_cret: 各期累積異常報酬
  Ef_scr <- mean(sret, na.rm = TRUE)
}

# Ef_mcr() 期末(n日)市場累積報酬率
Ef_mcr <- function(mydat, nowdate=Sys.Date(), startdate=as.Date("2005-01-01"), n1=0, n2=60){
  edate <- which(mydat$event$when<nowdate-n2 &
                   mydat$event$when>=startdate)
  mk.w <- mydat$market[,edate]
  m0 <- coredata(mk.w[as.character(n1)])
  mn <- coredata(mk.w[as.character(n2)])
  mret <- (mn-m0)/m0
  Ef_mcr <- mean(mret, na.rm = TRUE)
}

# Ef_winprb1() 期末(n日)累積報酬率大於零機率
Ef_winprb1 <- function(mydat, nowdate=Sys.Date(), startdate=as.Date("2005-01-01"), n1=0, n2=60){
  edate <- which(mydat$event$when<nowdate-n2 &
                   mydat$event$when>=startdate)
  es.w <- mydat$stock[,edate]
  s0 <- coredata(es.w[as.character(n1)])
  sn <- coredata(es.w[as.character(n2)])
  sret <- (sn-s0)/s0

  winmatrix <- sret
  winmatrix[sret>=0] <- 1
  winmatrix[sret<0] <- 0

  Ef_winprb1 <- sum(winmatrix, na.rm = TRUE)/length(winmatrix)
}

# Ef_winprb2() 期末(n日)累積報酬率超越市場機率
Ef_winprb2 <- function(mydat, nowdate=Sys.Date(), startdate=as.Date("2005-01-01"), n1=0, n2=60){
  edate <- which(mydat$event$when<nowdate-n2 &
                   mydat$event$when>=startdate)
  es.w <- mydat$stock[,edate]
  mk.w <- mydat$market[,edate]
  s0 <- coredata(es.w[as.character(n1)])
  m0 <- coredata(mk.w[as.character(n1)])

  sn <- coredata(es.w[as.character(n2)])
  mn <- coredata(mk.w[as.character(n2)])

  sret <- (sn-s0)/s0
  mret <- (mn-m0)/m0
  aret <- sret-mret

  winmatrix <- aret
  winmatrix[aret>=0] <- 1
  winmatrix[aret<0] <- 0

  Ef_winprb2 <- sum(winmatrix, na.rm = TRUE)/length(winmatrix)
}

# Ef_carquat() 期末(n日)累積異常報酬率quantile
Ef_carquat <- function(mydat, nowdate=Sys.Date(), startdate=as.Date("2005-01-01"), n1=0, n2=60, quat=0.5){
  edate <- which(mydat$event$when<nowdate-n2 &
                   mydat$event$when>=startdate)
  event <- mydat$event[edate,]
  es.w <- mydat$stock[,edate]
  mk.w <- mydat$market[,edate]

  s0 <- coredata(es.w[as.character(n1)])
  m0 <- coredata(mk.w[as.character(n1)])

  sn <- coredata(es.w[as.character(n2)])
  mn <- coredata(mk.w[as.character(n2)])

  sret <- (sn-s0)/s0
  mret <- (mn-m0)/m0
  aret <- sret-mret

  # adj_cret: 各期累積異常報酬
  Ef_carquat <- as.numeric(quantile(aret,probs=quat, na.rm = TRUE))
}

# Ef_scrquat() 期末(n日)累積異常報酬率quantile
Ef_scrquat <- function(mydat, nowdate=Sys.Date(), startdate=as.Date("2005-01-01"), n1=0, n2=60, quat=0.5){
  edate <- which(mydat$event$when<nowdate-n2 &
                   mydat$event$when>=startdate)
  event <- mydat$event[edate,]
  es.w <- mydat$stock[,edate]
  s0 <- coredata(es.w[as.character(n1)])
  sn <- coredata(es.w[as.character(n2)])
  sret <- (sn-s0)/s0

  # adj_cret: 各期累積異常報酬
  Ef_scrquat <- as.numeric(quantile(sret,probs=quat, na.rm = TRUE))
}

# Ef_mcrquat() 期末(n日)累積異常報酬率quantile
Ef_mcrquat <- function(mydat, nowdate=Sys.Date(), startdate=as.Date("2005-01-01"),n1=0,n2=60, quat=0.5){
  edate <- which(mydat$event$when<nowdate-n2 &
                   mydat$event$when>=startdate)
  mk.w <- mydat$market[,edate]
  m0 <- coredata(mk.w[as.character(n1)])
  mn <- coredata(mk.w[as.character(n2)])
  mret <- (mn-m0)/m0
  Ef_mcrquat <- as.numeric(quantile(mret,probs=quat, na.rm = TRUE))
}

# Ef_getallcret() 回傳n1-n2的累積報酬率  scr, mcr, event
Ef_getallcret <- function(mydat, nowdate=Sys.Date(), startdate=as.Date("2005-01-01"), n1=0, n2=60){
  edate <- which(mydat$event$when<nowdate-n2 &
                   mydat$event$when>=startdate)

  event <- mydat$event[edate,]
  es.w <- mydat$stock[,edate]
  mk.w <- mydat$market[,edate]

  s0 <- matrix(1,n2-n1)%*%coredata(es.w[as.character(n1)])
  m0 <- matrix(1,n2-n1)%*%coredata(mk.w[as.character(n1)])

  sn <- coredata(es.w[as.character((n1+1):n2)])
  mn <- coredata(mk.w[as.character((n1+1):n2)])

  sret <- (sn-s0)/s0
  mret <- (mn-m0)/m0
  # aret <- sret-mret

  # adj_cret: 各期累積異常報酬
  #adj_cret <- rowMeans(aret, na.rm = TRUE)
  # raw_cret: 各期未調整累積報酬
  #raw_cret <- rowMeans(sret, na.rm = TRUE)

  Ef_getallcret <- list(scr=sret, mcr=mret, events=event, epar=c(n1,n2))
}

# 清除事件发生后m日发生的重覆事件 (预设m=30)
Ef_dataclean <- function(mydat, m=30){
  edat <- mydat$event
  edat$ind <- seq(1, nrow(edat))
  edat$useindex <- rep(FALSE,nrow(edat))
  edat <- arrange(edat,name,when)

  # initial state
  tempid <- edat$name[1]
  tempdate <- edat$when[1]
  edat$useindex[1] <- TRUE

  for (i in 2:nrow(edat)){
    if (tempid != edat$name[i]){
      tempid <- edat$name[i]
      tempdate <- edat$when[i]
      edat$useindex[i] <- TRUE
    }else{
      if(tempdate < edat$when[i]-m){
        tempid <- edat$name[i]
        tempdate <- edat$when[i]
        edat$useindex[i] <- TRUE
      }
    }
  }

  edat <- arrange(edat,ind)
  mydat$event <- mydat$event[edat$useindex,]
  mydat$stock <- mydat$stock[,edat$useindex]
  mydat$market <- mydat$market[,edat$useindex]
  Ef_dataclean <- mydat
}
#mydat1 <- Ef_dataclean(mydat,30)
#temp <- arrange(mydat1$event,name,when)


