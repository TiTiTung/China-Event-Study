library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(highcharter)
library(shinythemes)
# 载入需要的事件函数
source("dataset/zfun.R")
# 将所有Events读入程序中

ctable <- data.frame(
  event <- c("概念股","定向增发","大宗交易","业绩上涨","战略投资",
             "战略合作","产能扩张","政府补助","提供担保","业绩下跌"),
  fname <- c("Event001.rds","Event002.rds","Event003.rds","Event004.rds","Event005.rds",
             "Event006.rds","Event007.rds","Event008.rds","Event009.rds","Event010.rds")
)

# 产业别数据
cdat <- readRDS("dataset/TRD_Co.rds") %>%
  mutate(Stkcd=paste0("X",Stkcd)) %>%
  select(Stkcd, Stknme, Indnme)

# 读入产生数据 (累积异常报酬)
getdata1 <- function(mydat,n1, n2) {
  n1 <- n1-1
  n2 <- n2
  fdat <- Ef_getallcret(mydat,n1=n1,n2=n2)
  fdat <- apply(fdat$scr-fdat$mcr,1,mean,na.rm = TRUE)
  fdat <- data.frame(time=seq((n1+1),n2),ret=round(fdat,3))
  getdata1 <- fdat
}
# 读入产生数据 (累积原始报酬)
getdata1s <- function(mydat,n1, n2) {
  n1 <- n1-1
  n2 <- n2
  fdat <- Ef_getallcret(mydat,n1=n1,n2=n2)
  fdat <- apply(fdat$scr,1,mean,na.rm = TRUE)
  fdat <- data.frame(time=seq((n1+1),n2),ret=round(fdat,3))
  getdata1s <- fdat
}
# 读入产生数据 (累积市场报酬)
getdata1m <- function(mydat,n1, n2) {
  n1 <- n1-1
  n2 <- n2
  fdat <- Ef_getallcret(mydat,n1=n1,n2=n2)
  fdat <- apply(fdat$mcr,1,mean,na.rm = TRUE)
  fdat <- data.frame(time=seq((n1+1),n2),ret=round(fdat,3))
  getdata1m <- fdat
}

# 投资分析
invan1 <- function(mydat,n1, n2, cvalue, critical){
  n1 <- n1-1
  n2 <- n2
  fdat <- Ef_getallcret(mydat,n1=n1,n2=n2)
  car <- fdat$scr-fdat$mcr
  # cvalue=0 追高策略，cvalue=1 反转策略
  if (cvalue==0){
    car <- car[,car[abs(n1),]>=critical]
  }else{
    car <- car[,car[abs(n1),]<critical]
  }

  fdat <- apply(car,1,mean,na.rm = TRUE)
  invan1 <- data.frame(time=seq((n1+1),n2),ret=round(fdat,3))
}

# 策略分析 (異常報酬)
invan2 <- function(mydat,n1, n2, cvalue, critical){
  n1 <- n1-1
  n2 <- n2
  fdat <- Ef_getallcret(mydat,n1=n1,n2=n2)
  car <- fdat$scr-fdat$mcr
  # cvalue=0 追高策略，cvalue=1 反转策略
  if (cvalue==0){
    car <- car[,car[abs(n1),]>=critical]
  }else{
    car <- car[,car[abs(n1),]<critical]
  }

  fdat <- apply(car,1,mean,na.rm = TRUE)
  fdat <- (1+fdat[abs(n1):(n2-n1)])/(1+fdat[abs(n1)])-1
  invan2 <- data.frame(time=seq(0,n2),ret=round(fdat,3))
}


# 策略分析 (原始報酬)
invan3 <- function(mydat,n1, n2, cvalue, critical){
  n1 <- n1-1
  n2 <- n2
  fdat <- Ef_getallcret(mydat,n1=n1,n2=n2)
  car <- fdat$scr
  # cvalue=0 追高策略，cvalue=1 反转策略
  if (cvalue==0){
    car <- car[,car[abs(n1),]>=critical]
  }else{
    car <- car[,car[abs(n1),]<critical]
  }

  fdat <- apply(car,1,mean,na.rm = TRUE)
  fdat <- (1+fdat[abs(n1):(n2-n1)])/(1+fdat[abs(n1)])-1
  invan3 <- data.frame(time=seq(0,n2),ret=round(fdat,3))
}



# 回传事件不同年份的数量
event_p1 <- function(ename){
  mydat1 <- readRDS(paste0("dataset/",ename))
  mydat1 <- Ef_dataclean(mydat1,10)$event

  event_p1 <- mydat1 %>%
    mutate(Year=format(when,"%Y"))%>%
    group_by(Year) %>%
    summarise(n=n())
}

# 回传事件不同产业的数量
event_p2 <- function(mydat2){
  cdat1 <- cdat %>%
    select(Indnme, Stkcd)
  mydat2 <- merge(x=mydat2$event, y=cdat1,
                 by.x="name", by.y="Stkcd", all.x=TRUE)

  event_p2 <- mydat2 %>%
    group_by(Indnme) %>%
    summarise(n=n())
}

# 回传quantile
event_quantile <- function(data1,q){
  event_quantile <- quantile(data1,q)
}

# 短期绩效衡量1 (异常报酬率)
event_p3 <- function(mydat3,n2){
  a3 <- Ef_getallcret(mydat3,n1=0,n2=n2)
  event_p3 <- a3$scr[n2,]-a3$mcr[n2,]
}


# *************************************
# 使用者介面 ui

ui <- navbarPage(
  theme = shinytheme("cerulean"),
  "智能事件趋动投资系统",
  # ***********************
  # 第1个分页
  tabPanel("1. 选择事件",
  sidebarLayout(
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      # Input: Simple integer interval ----
      selectInput("event_select", label = h4("选择特定的事件进行分析"),
                  choices = list("概念股" = "Event001.rds",
                                 "定向增发" = "Event002.rds",
                                 "大宗交易" = "Event003.rds",
                                 "业绩上涨" = "Event004.rds",
                                 "战略投资" = "Event005.rds",
                                 "战略合作" = "Event006.rds",
                                 "产能扩张" = "Event007.rds",
                                 "政府补助" = "Event008.rds",
                                 "提供担保" = "Event009.rds",
                                 "业绩下跌" = "Event010.rds"),
                  selected = 1),
      hr(),
      highchartOutput("hcontainer2",height = "200px"),
      hr(),
      highchartOutput("hcontainer3",height = "200px")
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      titlePanel("事件完整数据表"),
      hr(),
      DT::dataTableOutput("table")
    ))
  ),
  # ***********************
  # 第2个分页
  tabPanel("2. 短期绩效",
           sidebarLayout(
             # Sidebar to demonstrate various slider options ----
             sidebarPanel(
               h4(textOutput("shtdesp")),
               hr(),
               helpText(HTML("<font size=4><b>报酬分析</i></b></font>")),
               h5(textOutput("shtvalue1")),
               h5(textOutput("shtvalue5")),
               h5(textOutput("shtvalue6")),
               h5(textOutput("shtvalue3")),
               hr(),
               helpText(HTML("<font size=4><b>风险分析</b></font>")),
               h5(textOutput("shtvalue2")),
               h5(textOutput("shtvalue4")),
               h5(textOutput("shtvalue7"))
             ),
             # Main panel for displaying outputs ----
             mainPanel(
               # Output: Tabset w/ plot, summary, and table ----
               tabsetPanel(type = "tabs",
                           tabPanel("报酬率分布", highchartOutput("shorterm1",height = "400px",width = "500px")),
                           tabPanel("行业报酬率", highchartOutput("shorterm2",height = "400px",width = "500px")),
                           tabPanel("行业波动率", highchartOutput("shorterm3",height = "400px",width = "500px")),
                           tabPanel("各年报酬率", highchartOutput("shorterm4",height = "400px",width = "500px")),
                           tabPanel("各年波动率", highchartOutput("shorterm5",height = "400px",width = "500px"))
               )
             ))
           ),
  # ***********************
  # 第3个分页
  tabPanel("3. 长期绩效",
           sidebarLayout(
             # Sidebar to demonstrate various slider options ----
             sidebarPanel(
               h4(textOutput("longdesp")),
               hr(),
               helpText(HTML("<font size=4><b>报酬分析</i></b></font>")),
               h5(textOutput("longvalue1")),
               h5(textOutput("longvalue5")),
               h5(textOutput("longvalue6")),
               h5(textOutput("longvalue3")),
               hr(),
               helpText(HTML("<font size=4><b>风险分析</b></font>")),
               h5(textOutput("longvalue2")),
               h5(textOutput("longvalue4")),
               h5(textOutput("longvalue7"))
             ),
             # Main panel for displaying outputs ----
             mainPanel(
               # Output: Tabset w/ plot, summary, and table ----
               tabsetPanel(type = "tabs",
                           tabPanel("报酬率分布", highchartOutput("longterm1",height = "400px",width = "500px")),
                           tabPanel("行业报酬率", highchartOutput("longterm2",height = "400px",width = "500px")),
                           tabPanel("行业波动率", highchartOutput("longterm3",height = "400px",width = "500px")),
                           tabPanel("各年报酬率", highchartOutput("longterm4",height = "400px",width = "500px")),
                           tabPanel("各年波动率", highchartOutput("longterm5",height = "400px",width = "500px"))
               )
             ))
  ),
  # ***********************
  # 第4个分页
  tabPanel("4. 投资分析",
           sidebarLayout(
             sidebarPanel(h4(textOutput("invdesp")),
                          hr(),
                          h5(textOutput("invdesp2")),
                          numericInput("invnum1",
                                       label = h5("动能参数设置 (%)"),
                                       value = 0, min=-20, max=20),
                          helpText("参数为0%表示若从n1日到事件日的累积报酬率大于0%则买入，否则不进行任何动作。"),
                          numericInput("invnum2",
                                        label = h5("反向参数设置 (%)"),
                                        value = 0,min=-20, max=20),
                          helpText("参数为0%表示若从n1日到事件日的累积报酬率小于0%则买入，否则不进行任何动作。")
                          ),
                       mainPanel(
                        tabsetPanel(type = "tabs",
                          tabPanel("报酬率分析", highchartOutput("advhc1",height = "400px",width = "500px")),
                          tabPanel("策略汇总图 (异常报酬)",highchartOutput("advhc2",height = "400px",width = "500px")),
                          tabPanel("策略汇总图 (原始报酬)", highchartOutput("advhc3",height = "400px",width = "500px"))
                       ))
           )
  ),


  # ***********************
  # 第5个分页
  tabPanel("5. 系统设置",
           themeSelector(),
           # 计算短期绩效天数
           numericInput("snum1", label = h5("设置短期绩效天数 (3-19日)"), value = 10,
                        min=5, max=19),
           hr(),
           numericInput("lnum1", label = h5("设置长期绩效天数 (20-90日)"), value = 60,
                        min=20, max=90),
           hr(),
           numericInput("advnum1", label = h5("投资分析:事件窗口 (左侧，0日到-60日)"), min = -120,
                       max = 0, value = -60),
           numericInput("advnum2", label = h5("投资分析:事件窗口 (右侧5日到120日)"), min = 5,
                       max = 120, value = 60),
           hr(),
           selectInput("figstyle", label = h5("图形风格"),
                       choices = list("hc_theme_538" = 1,
                                      "hc_theme_chalk" = 2,
                                      "hc_theme_darkunica" = 3,
                                      "hc_theme_db" = 4,
                                      "hc_theme_economist" = 5,
                                      "hc_theme_elementary" = 6,
                                      "hc_theme_ffx" = 7,
                                      "hc_theme_flat" = 8,
                                      "hc_theme_flatdark" = 9,
                                      "hc_theme_ft" = 10,
                                      "hc_theme_google" = 11,
                                      "hc_theme_gridlight" = 12,
                                      "hc_theme_handdrawn" = 13,
                                      "hc_theme_monokai" = 14,
                                      "hc_theme_sandsignika" = 15,
                                      "hc_theme_smpl" = 16
                       ),
                       selected = 11),
           hr()
           )
)

# ********************************************************
# server 端设定

server <- function(input, output) {
  # ************************
  # Data Input

  dataInput1 <- reactive({
    Ef_dataclean(readRDS(paste0("dataset/",input$event_select)),10)
  })

  dataInput2 <- reactive({
   sdat <- dataInput1()
   mydat <- merge(x=sdat$event, y=cdat,
                  by.x="name", by.y="Stkcd", all.x=TRUE) %>%
          arrange(when, name)
   mydat$name <- gsub("X","",mydat$name)
   names(mydat) <- c("个股代码", "事件日期", "个股简称", "行业")
   mydat
  })

  dataInput3 <- reactive({
    sdat <- dataInput1()
    mydat <- merge(x=sdat$event, y=cdat,
                   by.x="name", by.y="Stkcd", all.x=TRUE)
    mydat$name <- gsub("X","",mydat$name)
    #names(mydat) <- c("个股代码", "事件日期", "个股简称", "行业")
    mydat <- data.frame(mydat, car=round(event_p3(dataInput1(),input$snum1),3))
    mydat %>%
      group_by(Indnme) %>%
      summarise(mean=mean(car,na.rm=TRUE),num=n(),sd=sd(car,na.rm=TRUE),q1=quantile(car,0.05,na.rm=TRUE))
  })

  dataInput3L <- reactive({
    sdat <- dataInput1()
    mydat <- merge(x=sdat$event, y=cdat,
                   by.x="name", by.y="Stkcd", all.x=TRUE)
    mydat$name <- gsub("X","",mydat$name)
    #names(mydat) <- c("个股代码", "事件日期", "个股简称", "行业")
    mydat <- data.frame(mydat, car=round(event_p3(dataInput1(),input$lnum1),3))
    mydat %>%
      group_by(Indnme) %>%
      summarise(mean=mean(car,na.rm=TRUE),num=n(),sd=sd(car,na.rm=TRUE),q1=quantile(car,0.05,na.rm=TRUE))
  })

  dataInput4 <- reactive({
    sdat <- dataInput1()
    mydat <- merge(x=sdat$event, y=cdat,
                   by.x="name", by.y="Stkcd", all.x=TRUE)
    mydat$name <- gsub("X","",mydat$name)
    mydat <- data.frame(mydat, car=round(event_p3(dataInput1(),input$snum1),3))
    mydat <- mydat %>%
      group_by(format(when,"%Y")) %>%
      summarise(mean=mean(car,na.rm=TRUE),num=n(),sd=sd(car,na.rm=TRUE),q1=quantile(car,0.05,na.rm=TRUE))
    names(mydat) <- c("when", "car", "num", "sd", "q1")
    mydat
  })

  dataInput4L <- reactive({
    sdat <- dataInput1()
    mydat <- merge(x=sdat$event, y=cdat,
                   by.x="name", by.y="Stkcd", all.x=TRUE)
    mydat$name <- gsub("X","",mydat$name)
    mydat <- data.frame(mydat, car=round(event_p3(dataInput1(),input$lnum1),3))
    mydat <- mydat %>%
      group_by(format(when,"%Y")) %>%
      summarise(mean=mean(car,na.rm=TRUE),num=n(),sd=sd(car,na.rm=TRUE),q1=quantile(car,0.05,na.rm=TRUE))
    names(mydat) <- c("when", "car", "num", "sd", "q1")
    mydat
  })

  # 短期绩效用的数据
  shortdata <- reactive(
    event_p3(dataInput1(),input$snum1)
  )
  # 长期绩效用的数据
  longdata <- reactive(
    event_p3(dataInput1(),input$lnum1)
  )
  # DATATable数据
  output$table <- DT::renderDataTable(DT::datatable(
    dataInput2(), options = list(searching = FALSE)
  ))


  # ************************
  # hcontainer2
    output$hcontainer2 <- renderHighchart(
      highchart() %>%
        hc_add_series(event_p1(input$event_select), "pie",
                      hcaes(name = Year, y = n), name = "pie") %>%
        hc_title(
          text = paste0("事件发生",sum(event_p1(input$event_select)$n),"次"),
          align = "center"
        ) %>%
        hc_subtitle(text = "不同年份事件发生次数",
                    align = "center")
    )

    # ************************
    # hcontainer3
    output$hcontainer3 <- renderHighchart(
      highchart() %>%
       hc_add_series(event_p2(dataInput1()), "pie", hcaes(name = Indnme, y = n), name = "pie") %>%
        hc_subtitle(text = "不同行业别事件发生次数",
                    align = "center")%>%
        hc_credits(enabled = TRUE,
                   text = "© IEDI Group 2018",
                   style = list(fontSize = "10px"))
    )

    # ************************
    # 短期绩效图1
    output$shorterm1 <- renderHighchart(
      hchist(shortdata(),shadow=TRUE,borderWidth=3,
             borderColor="#ff00ff",
             showInLegend = FALSE)%>%
        hc_title(text = paste0(input$snum1,"日累积异常报酬分布图"),
                 margin = 30,align = "center") %>%
        hc_subtitle(text = as.character(ctable[ctable$fname==input$event_select,]$event),
                    align = "center") %>%
        hc_credits(enabled = TRUE,
                   text = "© IEDI Group 2018",
                   style = list(fontSize = "10px"))%>%
        hc_xAxis(title = list(text = "累积异常报酬率"),
                 plotLines = list(
                             list(label = list(text = "95%风险值"),
                             color = "#FF0000",
                             width = 2,
                             value = as.numeric(quantile(shortdata(),0.05))
                             )))%>%
        hc_add_theme(switch(as.numeric(input$figstyle),
                            hc_theme_538(),
                            hc_theme_chalk(),
                            hc_theme_darkunica(),
                            hc_theme_db(),
                            hc_theme_economist(),
                            hc_theme_elementary(),
                            hc_theme_ffx(),
                            hc_theme_flat(),
                            hc_theme_flatdark(),
                            hc_theme_ft(),
                            hc_theme_google(),
                            hc_theme_gridlight(),
                            hc_theme_handdrawn(),
                            hc_theme_monokai(),
                            hc_theme_sandsignika(),
                            hc_theme_smpl()
        )))


    # ************************
    # 短期绩效图2
    output$shorterm2 <- renderHighchart(
      # highcharts_demo()
      #highchart() %>%
      #  hc_add_series_scatter(x = dataInput3()$sd, y = dataInput3()$mean)
      highchart() %>%
        hc_add_series(dataInput3(), "column",
                      hcaes(x = Indnme, y = round(mean,3), color=Indnme),
                      name="累积异常报酬", showInLegend = FALSE) %>%
        hc_xAxis(categories = dataInput3()$Indnme)%>%
        hc_title(text = paste0("行业",input$snum1,"日累积异常报酬图"),
                 margin = 30,align = "center") %>%
        hc_subtitle(text = as.character(ctable[ctable$fname==input$event_select,]$event),
                    align = "center") %>%
        hc_credits(enabled = TRUE,
                   text = "© IEDI Group 2018",
                   style = list(fontSize = "10px"))%>%
        hc_add_theme(switch(as.numeric(input$figstyle),
                            hc_theme_538(),
                            hc_theme_chalk(),
                            hc_theme_darkunica(),
                            hc_theme_db(),
                            hc_theme_economist(),
                            hc_theme_elementary(),
                            hc_theme_ffx(),
                            hc_theme_flat(),
                            hc_theme_flatdark(),
                            hc_theme_ft(),
                            hc_theme_google(),
                            hc_theme_gridlight(),
                            hc_theme_handdrawn(),
                            hc_theme_monokai(),
                            hc_theme_sandsignika(),
                            hc_theme_smpl()
        ))
    )

    # ************************
    # 短期绩效图3
    output$shorterm3 <- renderHighchart(
      highchart() %>%
        hc_add_series(dataInput3(), "column",
                      hcaes(x = Indnme, y = round(sd,3), color=Indnme),
                      name="累积异常报酬", showInLegend = FALSE) %>%
        hc_xAxis(categories = dataInput3()$Indnme)%>%
        hc_title(text = paste0("行业",input$snum1,"日波动率图"),
                 margin = 30,align = "center") %>%
        hc_subtitle(text = as.character(ctable[ctable$fname==input$event_select,]$event),
                    align = "center") %>%
        hc_credits(enabled = TRUE,
                   text = "© IEDI Group 2018",
                   style = list(fontSize = "10px"))%>%
        hc_add_theme(switch(as.numeric(input$figstyle),
                            hc_theme_538(),
                            hc_theme_chalk(),
                            hc_theme_darkunica(),
                            hc_theme_db(),
                            hc_theme_economist(),
                            hc_theme_elementary(),
                            hc_theme_ffx(),
                            hc_theme_flat(),
                            hc_theme_flatdark(),
                            hc_theme_ft(),
                            hc_theme_google(),
                            hc_theme_gridlight(),
                            hc_theme_handdrawn(),
                            hc_theme_monokai(),
                            hc_theme_sandsignika(),
                            hc_theme_smpl()
        ))
    )

    # ************************
    # 短期绩效图4
    output$shorterm4 <- renderHighchart(
      highchart() %>%
        hc_add_series(dataInput4(), "line",
                      hcaes(x = when, y = round(car,3), color=when),
                      name="累积异常报酬", showInLegend = FALSE) %>%
        hc_xAxis(categories = dataInput4()$when)%>%
        hc_title(text = paste0("各年", input$snum1,"日累积异常报酬率图"),
                 margin = 30,align = "center") %>%
        hc_subtitle(text = as.character(ctable[ctable$fname==input$event_select,]$event),
                    align = "center") %>%
        hc_credits(enabled = TRUE,
                   text = "© IEDI Group 2018",
                   style = list(fontSize = "10px"))%>%
        hc_add_theme(switch(as.numeric(input$figstyle),
                            hc_theme_538(),
                            hc_theme_chalk(),
                            hc_theme_darkunica(),
                            hc_theme_db(),
                            hc_theme_economist(),
                            hc_theme_elementary(),
                            hc_theme_ffx(),
                            hc_theme_flat(),
                            hc_theme_flatdark(),
                            hc_theme_ft(),
                            hc_theme_google(),
                            hc_theme_gridlight(),
                            hc_theme_handdrawn(),
                            hc_theme_monokai(),
                            hc_theme_sandsignika(),
                            hc_theme_smpl()
        ))
    )
    # ************************
    # 短期绩效图5
    output$shorterm5 <- renderHighchart(
      highchart() %>%
        hc_add_series(dataInput4(), "line",
                      hcaes(x = when, y = round(sd,3), color=when),
                      name="累积异常报酬", showInLegend = FALSE) %>%
        hc_xAxis(categories = dataInput4()$when)%>%
        hc_title(text = paste0("各年", input$snum1,"日波动率图"),
                 margin = 30,align = "center") %>%
        hc_subtitle(text = as.character(ctable[ctable$fname==input$event_select,]$event),
                    align = "center") %>%
        hc_credits(enabled = TRUE,
                   text = "© IEDI Group 2018",
                   style = list(fontSize = "10px"))%>%
        hc_add_theme(switch(as.numeric(input$figstyle),
                            hc_theme_538(),
                            hc_theme_chalk(),
                            hc_theme_darkunica(),
                            hc_theme_db(),
                            hc_theme_economist(),
                            hc_theme_elementary(),
                            hc_theme_ffx(),
                            hc_theme_flat(),
                            hc_theme_flatdark(),
                            hc_theme_ft(),
                            hc_theme_google(),
                            hc_theme_gridlight(),
                            hc_theme_handdrawn(),
                            hc_theme_monokai(),
                            hc_theme_sandsignika(),
                            hc_theme_smpl()
        ))
    )

    # ************************
    # shorterm1
    output$shtdesp <- renderText({
      paste0("事件－", ctable[ctable$fname==input$event_select,]$event, " (", input$snum1, "日)")
    })

    output$shtvalue1 <- renderText({
      paste0("累积异常报酬率: ",
             round(Ef_car(dataInput1(),n1=0,n2=input$snum1)[1]*100,2), "%")
       })
    output$shtvalue5 <- renderText({
      paste0("个股累积报酬率: ",
             round(Ef_scr(dataInput1(),n1=0,n2=input$snum1)[1]*100,2), "%")
    })
    output$shtvalue6 <- renderText({
      paste0("市场累积报酬率: ",
             round(Ef_mcr(dataInput1(),n1=0,n2=input$snum1)[1]*100,2), "%")
    })
    output$shtvalue2 <- renderText({
      paste0("期末胜率: ",
             round(Ef_winprb1(dataInput1(),n1=0,n2=input$snum1)[1]*100,2),"%") })
    output$shtvalue3 <- renderText({
      paste0("万份收益: ",
             round(Ef_car(dataInput1(),n1=0,n2=input$snum1)[1]*10000,2),"元") })

    output$shtvalue4 <- renderText({
      paste0("个股", input$snum1, "日波动率: ",
             round(sd(shortdata())*100,2),"%") })
    output$shtvalue7 <- renderText({
      paste0("95%风险值: ",
             round(quantile(shortdata(),0.05)*100,2),"%") })



    # ************************
    # 长期绩效图1
    output$longterm1 <- renderHighchart(
      hchist(longdata(),shadow=TRUE,borderWidth=3,
             borderColor="#ff00ff",
             showInLegend = FALSE)%>%
        hc_title(text = paste0(input$lnum1,"日累积异常报酬分布图"),
                 margin = 30,align = "center") %>%
        hc_subtitle(text = as.character(ctable[ctable$fname==input$event_select,]$event),
                    align = "center") %>%
        hc_credits(enabled = TRUE,
                   text = "© IEDI Group 2018",
                   style = list(fontSize = "10px"))%>%
        hc_xAxis(title = list(text = "累积异常报酬率"),
                 plotLines = list(
                   list(label = list(text = "95%风险值"),
                        color = "#FF0000",
                        width = 2,
                        value = as.numeric(quantile(longdata(),0.05,na.rm=TRUE))
                   )))%>%
        hc_add_theme(switch(as.numeric(input$figstyle),
                            hc_theme_538(),
                            hc_theme_chalk(),
                            hc_theme_darkunica(),
                            hc_theme_db(),
                            hc_theme_economist(),
                            hc_theme_elementary(),
                            hc_theme_ffx(),
                            hc_theme_flat(),
                            hc_theme_flatdark(),
                            hc_theme_ft(),
                            hc_theme_google(),
                            hc_theme_gridlight(),
                            hc_theme_handdrawn(),
                            hc_theme_monokai(),
                            hc_theme_sandsignika(),
                            hc_theme_smpl()
        )))

    # ************************
    # 长期绩效图2
    output$longterm2 <- renderHighchart(
      highchart() %>%
        hc_add_series(dataInput3L(), "column",
                      hcaes(x = Indnme, y = round(mean,3), color=Indnme),
                      name="累积异常报酬", showInLegend = FALSE) %>%
        hc_xAxis(categories = dataInput3L()$Indnme)%>%
        hc_title(text = paste0("行业",input$lnum1,"日累积异常报酬图"),
                 margin = 30,align = "center") %>%
        hc_subtitle(text = as.character(ctable[ctable$fname==input$event_select,]$event),
                    align = "center") %>%
        hc_credits(enabled = TRUE,
                   text = "© IEDI Group 2018",
                   style = list(fontSize = "10px"))%>%
        hc_add_theme(switch(as.numeric(input$figstyle),
                            hc_theme_538(),
                            hc_theme_chalk(),
                            hc_theme_darkunica(),
                            hc_theme_db(),
                            hc_theme_economist(),
                            hc_theme_elementary(),
                            hc_theme_ffx(),
                            hc_theme_flat(),
                            hc_theme_flatdark(),
                            hc_theme_ft(),
                            hc_theme_google(),
                            hc_theme_gridlight(),
                            hc_theme_handdrawn(),
                            hc_theme_monokai(),
                            hc_theme_sandsignika(),
                            hc_theme_smpl()
        ))
    )

    # ************************
    # 长期绩效图3
    output$longterm3 <- renderHighchart(
      highchart() %>%
        hc_add_series(dataInput3L(), "column",
                      hcaes(x = Indnme, y = round(sd,3), color=Indnme),
                      name="累积异常报酬", showInLegend = FALSE) %>%
        hc_xAxis(categories = dataInput3L()$Indnme)%>%
        hc_title(text = paste0("行业",input$lnum1,"日波动率图"),
                 margin = 30,align = "center") %>%
        hc_subtitle(text = as.character(ctable[ctable$fname==input$event_select,]$event),
                    align = "center") %>%
        hc_credits(enabled = TRUE,
                   text = "© IEDI Group 2018",
                   style = list(fontSize = "10px"))%>%
        hc_add_theme(switch(as.numeric(input$figstyle),
                            hc_theme_538(),
                            hc_theme_chalk(),
                            hc_theme_darkunica(),
                            hc_theme_db(),
                            hc_theme_economist(),
                            hc_theme_elementary(),
                            hc_theme_ffx(),
                            hc_theme_flat(),
                            hc_theme_flatdark(),
                            hc_theme_ft(),
                            hc_theme_google(),
                            hc_theme_gridlight(),
                            hc_theme_handdrawn(),
                            hc_theme_monokai(),
                            hc_theme_sandsignika(),
                            hc_theme_smpl()
        ))
    )

    # ************************
    # 长期绩效图4
    output$longterm4 <- renderHighchart(
      highchart() %>%
        hc_add_series(dataInput4L(), "line",
                      hcaes(x = when, y = round(car,3), color=when),
                      name="累积异常报酬", showInLegend = FALSE) %>%
        hc_xAxis(categories = dataInput4()$when)%>%
        hc_title(text = paste0("各年", input$lnum1,"日累积异常报酬率图"),
                 margin = 30,align = "center") %>%
        hc_subtitle(text = as.character(ctable[ctable$fname==input$event_select,]$event),
                    align = "center") %>%
        hc_credits(enabled = TRUE,
                   text = "© IEDI Group 2018",
                   style = list(fontSize = "10px"))%>%
        hc_add_theme(switch(as.numeric(input$figstyle),
                            hc_theme_538(),
                            hc_theme_chalk(),
                            hc_theme_darkunica(),
                            hc_theme_db(),
                            hc_theme_economist(),
                            hc_theme_elementary(),
                            hc_theme_ffx(),
                            hc_theme_flat(),
                            hc_theme_flatdark(),
                            hc_theme_ft(),
                            hc_theme_google(),
                            hc_theme_gridlight(),
                            hc_theme_handdrawn(),
                            hc_theme_monokai(),
                            hc_theme_sandsignika(),
                            hc_theme_smpl()
        ))
    )
    # ************************
    # 长期绩效图5
    output$longterm5 <- renderHighchart(
      highchart() %>%
        hc_add_series(dataInput4L(), "line",
                      hcaes(x = when, y = round(sd,3), color=when),
                      name="累积异常报酬", showInLegend = FALSE) %>%
        hc_xAxis(categories = dataInput4L()$when)%>%
        hc_title(text = paste0("各年", input$lnum1,"日波动率图"),
                 margin = 30,align = "center") %>%
        hc_subtitle(text = as.character(ctable[ctable$fname==input$event_select,]$event),
                    align = "center") %>%
        hc_credits(enabled = TRUE,
                   text = "© IEDI Group 2018",
                   style = list(fontSize = "10px"))%>%
        hc_add_theme(switch(as.numeric(input$figstyle),
                            hc_theme_538(),
                            hc_theme_chalk(),
                            hc_theme_darkunica(),
                            hc_theme_db(),
                            hc_theme_economist(),
                            hc_theme_elementary(),
                            hc_theme_ffx(),
                            hc_theme_flat(),
                            hc_theme_flatdark(),
                            hc_theme_ft(),
                            hc_theme_google(),
                            hc_theme_gridlight(),
                            hc_theme_handdrawn(),
                            hc_theme_monokai(),
                            hc_theme_sandsignika(),
                            hc_theme_smpl()
        ))
    )





    # ************************
    # longterm1
    output$longdesp <- renderText({
      paste0("事件－", ctable[ctable$fname==input$event_select,]$event, " (", input$lnum1, "日)")
    })

    output$longvalue1 <- renderText({
      paste0("累积异常报酬率: ",
             round(Ef_car(dataInput1(),n1=0,n2=input$lnum1)[1]*100,2), "%")
    })
    output$longvalue5 <- renderText({
      paste0("个股累积报酬率: ",
             round(Ef_scr(dataInput1(),n1=0,n2=input$lnum1)[1]*100,2), "%")
    })
    output$longvalue6 <- renderText({
      paste0("市场累积报酬率: ",
             round(Ef_mcr(dataInput1(),n1=0,n2=input$lnum1)[1]*100,2), "%")
    })
    output$longvalue2 <- renderText({
      paste0("期末胜率: ",
             round(Ef_winprb1(dataInput1(),n1=0,n2=input$lnum1)[1]*100,2),"%") })
    output$longvalue3 <- renderText({
      paste0("万份收益: ",
             round(Ef_car(dataInput1(),n1=0,n2=input$lnum1)[1]*10000,2),"元") })

    output$longvalue4 <- renderText({
      paste0("个股", input$lnum1, "日波动率: ",
             round(sd(longdata(),na.rm=TRUE)*100,2),"%") })
    output$longvalue7 <- renderText({
      paste0("95%风险值: ",
             round(quantile(longdata(),0.05,na.rm=TRUE)*100,2),"%") })


    # ************************
    # advhc1
    output$advhc1 <- renderHighchart(
      highchart() %>%
        hc_add_series(getdata1(dataInput1(),n1=input$advnum1,n2=input$advnum2),
                      "area", hcaes(x = time, y = ret),
                      showInLegend = TRUE, name="异常报酬",
                      size="20%")%>%
        hc_add_series(getdata1m(dataInput1(),n1=input$advnum1,n2=input$advnum2),
                      "line", hcaes(x = time, y = ret),
                      showInLegend = TRUE, name="市场报酬")%>%
        hc_add_series(getdata1s(dataInput1(),n1=input$advnum1,n2=input$advnum2),
                      "line", hcaes(x = time, y = ret),
                      showInLegend = TRUE, name="个股报酬")%>%
        hc_title(text = "事件发生前后累积报酬分析图",margin = 30,align = "center") %>%
        hc_subtitle(text = as.character(ctable[ctable$fname==input$event_select,]$event),
                    align = "center") %>%
        hc_credits(enabled = TRUE,
                   text = "© IEDI Group 2018",
                   style = list(fontSize = "10px"))%>%
        hc_add_theme(switch(as.numeric(input$figstyle),
                            hc_theme_538(),
                            hc_theme_chalk(),
                            hc_theme_darkunica(),
                            hc_theme_db(),
                            hc_theme_economist(),
                            hc_theme_elementary(),
                            hc_theme_ffx(),
                            hc_theme_flat(),
                            hc_theme_flatdark(),
                            hc_theme_ft(),
                            hc_theme_google(),
                            hc_theme_gridlight(),
                            hc_theme_handdrawn(),
                            hc_theme_monokai(),
                            hc_theme_sandsignika(),
                            hc_theme_smpl()
        )
        )
    )


    # ************************
    # advhc2
    output$advhc2 <- renderHighchart(
      highchart() %>%
        hc_add_series(invan2(dataInput1(),n1=input$advnum1,n2=input$advnum2,0,-99),
                      "area", hcaes(x = time, y = ret),
                      showInLegend = TRUE, name="一般策略",
                      size="20%")%>%
        hc_add_series(invan2(dataInput1(),n1=input$advnum1,n2=input$advnum2,0,input$invnum1/100),
                      "line", hcaes(x = time, y = ret),
                      showInLegend = TRUE, name="动能策略")%>%
        hc_add_series(invan2(dataInput1(),n1=input$advnum1,n2=input$advnum2,1,input$invnum2/100),
                      "line", hcaes(x = time, y = ret),
                      showInLegend = TRUE, name="反向策略")%>%
        hc_title(text = "事件后累积异常报酬分析图",margin = 30,align = "center") %>%
        hc_subtitle(text = as.character(ctable[ctable$fname==input$event_select,]$event),
                    align = "center") %>%
        hc_credits(enabled = TRUE,
                   text = "© IEDI Group 2018",
                   style = list(fontSize = "10px"))%>%
        hc_add_theme(switch(as.numeric(input$figstyle),
                            hc_theme_538(),
                            hc_theme_chalk(),
                            hc_theme_darkunica(),
                            hc_theme_db(),
                            hc_theme_economist(),
                            hc_theme_elementary(),
                            hc_theme_ffx(),
                            hc_theme_flat(),
                            hc_theme_flatdark(),
                            hc_theme_ft(),
                            hc_theme_google(),
                            hc_theme_gridlight(),
                            hc_theme_handdrawn(),
                            hc_theme_monokai(),
                            hc_theme_sandsignika(),
                            hc_theme_smpl()
        )
        )
    )

    # ************************
    # advhc3
    output$advhc3 <- renderHighchart(
      highchart() %>%
        hc_add_series(invan3(dataInput1(),n1=input$advnum1,n2=input$advnum2,0,-99),
                      "area", hcaes(x = time, y = ret),
                      showInLegend = TRUE, name="一般策略",
                      size="20%")%>%
        hc_add_series(invan3(dataInput1(),n1=input$advnum1,n2=input$advnum2,0,input$invnum1/100),
                      "line", hcaes(x = time, y = ret),
                      showInLegend = TRUE, name="动能策略")%>%
        hc_add_series(invan3(dataInput1(),n1=input$advnum1,n2=input$advnum2,1,input$invnum2/100),
                      "line", hcaes(x = time, y = ret),
                      showInLegend = TRUE, name="反向策略")%>%
        hc_title(text = "事件后累积原始报酬分析图",margin = 30,align = "center") %>%
        hc_subtitle(text = as.character(ctable[ctable$fname==input$event_select,]$event),
                    align = "center") %>%
        hc_credits(enabled = TRUE,
                   text = "© IEDI Group 2018",
                   style = list(fontSize = "10px"))%>%
        hc_add_theme(switch(as.numeric(input$figstyle),
                            hc_theme_538(),
                            hc_theme_chalk(),
                            hc_theme_darkunica(),
                            hc_theme_db(),
                            hc_theme_economist(),
                            hc_theme_elementary(),
                            hc_theme_ffx(),
                            hc_theme_flat(),
                            hc_theme_flatdark(),
                            hc_theme_ft(),
                            hc_theme_google(),
                            hc_theme_gridlight(),
                            hc_theme_handdrawn(),
                            hc_theme_monokai(),
                            hc_theme_sandsignika(),
                            hc_theme_smpl()
        )
        )
    )

    # ************************
    # invdesp
    output$invdesp <- renderText({
      paste0(ctable[ctable$fname==input$event_select,]$event, ": 投资策略参数设置")
    })

    output$invdesp2 <- renderText({
      paste0("事件窗口左侧 (n1) 设置为", input$advnum1, "日")
    })

    # DATATable数据
    output$table_temp <- DT::renderDataTable(DT::datatable(
      invan2(dataInput1(),n1=input$advnum1,n2=input$advnum2,0,-99),
      options = list(searching = FALSE)
    ))




}

shinyApp(ui, server)
