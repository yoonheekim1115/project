rm(list=ls())
library(readxl)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(formattable)
year2010<-read.csv("C:/Users/Yoonhee/Desktop/수업자료, 과제/대학원/2학기/이통2/hw3/2010년 생명표.csv",header=TRUE, skip=1)

premium<-function(data,gender,age,rate,n, m, b){
  
  r<-log(1+rate)
  age<-age+1
  data<-data[1:100,]
  if(gender==0){
    lx<-data[,9]
  }else{
    lx<-data[,10]
  }
  
  lx_plus_n<-lx[(age+1):(age+(n-1))] 
  r_n<-r*seq(1:(n-1))
  ax_n<-0.5+ sum(lx_plus_n*exp(-r_n)/lx[age])+ 0.5*lx[(age+n)]*exp(-r*n)/lx[age]
  
  lx_plus_m<-lx[(age+1):(age+m)]
  r_m<-r*seq(1:m)
  Ax_m<-1-r*(0.5+sum(lx_plus_m*exp(-r_m))/lx[age]+(lx[age+m]/lx[age])*exp(-r*(m+1))/(1-exp(-r)))
  
  px_annual<-b*Ax_m/ax_n
  px<-px_annual/12
  result=list("px"=px_annual/12, "data"=data)
  return(result)
}  





calculate.px <- function(data, g, x, i, n, a, b){ # 데이터/성별/나이/이자율/납입만기/지급개시시접/월지급연급액
  
  # 생명표 데이터 
  if (g == 'f'){ # 여성
    Tx <- as.integer(data[,4][-1]) # 0세 제외해 1세부터 시작하도록
    l <- lx <- data[,10][-1]
  } else { # 남성
    Tx <- as.integer(data[,3][-1])
    l <- lx <- data[,9][-1]
  }
  
  r <- log(1+i)
  b <- b * 12 # 연 지급 연금액
  
  ## f(t) (trapezoidal rule)
  f <- function(t) {
    if (x+t > 100) { # 100세이상
      return(l[100]/l[x] * exp(-r*t))
    } else {
      return(l[x+t]/l[x] * exp(-r*t))
    }
  }
  
  ## sum of f(t)
  s <- function(start, end) {
    s <- 0
    for (t in start:end) {
      s <- s + f(t)
    }
    return(s)
  }
  
  ## integration - trapezoidal rule
  integration <- function(start, end) {
    (1/2)*f(start) + s(start, end) + (1/2)*f(end)
  }
  
  #expenditure <- px * integration(x, x+n) 현재나이~납입만기까지 지출
  
  annuity <- b * integration(a-x, Tx[x]) # 지급개시 시점~사망시까지 수입
  
  px <- annuity/integration(0, n)
  
  return(px/12) # 월납 연금보험료
  
}




#````````````````````````````````````````````````````````````````````````

## ui.R ##

header <- dashboardHeader(title = "Premium")
sidebar <- dashboardSidebar(
  sidebarMenu(
    fileInput('file1', label=h6('Choose CSV File'),
              accept=c('text/csv',  'text/comma-separated-values,text/plain', '.csv')),
    id = "tabs",
    
    radioButtons("year", "year", choices = list("2010" = 0, "other year" = 1), selected = 0),
    
    menuItem("Life insurance", tabName = "part1", icon = icon("heartbeat")),
    
    menuItem("Pension", tabName = "part2", icon = icon("users")),
    
    menuItemOutput("slider_sidebar"),
    
    menuItemOutput("Text_input")
  )
)


## Body
body <- dashboardBody(
  
  tabItems(
    tabItem("part1",  
            fluidRow(
              box(title="Gender", status="warning", width=4,height=150,solidHeader=TRUE,
                  radioButtons("Gender1", "" ,choices = list("male" = 0, "female" = 1), selected = 0)),
              box(title="Payment term", status="primary" ,width=4,height=150,solidHeader=TRUE,
                  numericInput("n1", "(year)", 20, 0, 100, 1, width=300)),
              box(title="Maturity term", status="primary", width=4,height=150,solidHeader=TRUE,
                  numericInput("m1", "(year)", 20, 0, 100, 1, width=300)))
            ,
            fluidRow(
              box(title="Age", status="warning", width=4,height=150,solidHeader=TRUE,
                  sliderInput("Age1","", 0, 100, 30)),
              box(title="Interest rate", status="primary", width=4,height=150,solidHeader=TRUE,
                  sliderInput("rate1", "(%)", 0, 20, 2)),
              box(title="Benefit", status="primary", width=4,height=150,solidHeader=TRUE,
                  numericInput("b1", "(10000 won)", 10000, 0, 20000, 1))
            ),
            fluidRow(
              valueBoxOutput("p1",width=12)
            )) 
    
    
    ,tabItem("part2",
             fluidRow(
               box(title ='Gender',width = 4, height = 200, solidHeader = TRUE, status = "warning",
                   radioButtons('Bgender',"", choices = list('Female'=0,'Male'=1),selected=1))
               ,
               box(title="Age", width = 4, height = 200, solidHeader = TRUE, status = "warning",
                   numericInput("Bage","",40,0,100,1))
               ,
               box(title = 'Retirement Age',width = 4, height = 200, solidHeader = TRUE, status = "warning",
                   numericInput("Br","",60,40,100,1))
               ),
             
             fluidRow(
               box(title = 'Interest Rate',width = 3, height = 200, solidHeader = TRUE, status = "primary",
                   sliderInput("Bi","",0,30,2)),
               box(title ='Payment Expiration',width = 3, height = 200, solidHeader = TRUE, status = "primary",  
                   sliderInput('Bn',"",0,50,30)),
               box(title='Provide Age',width = 3, height = 200, solidHeader = TRUE, status = "primary",
                   numericInput('Bm',"",70,40,100,1)),
               box(title='Monthly Receipt',width = 3, height = 200, solidHeader = TRUE, status = "primary",
                   sliderInput('Ba',"",0,500,100)),
             
             fluidRow(valueBoxOutput("Bp",width=12))
              ))
  ))



ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  
  fileupload<- reactive({
    inFile <- input$file1
    if (is.null(inFile)){
      return(NULL)
    }else {
      otheryear <- read.csv(inFile$datapath, header = TRUE,skip=1)
      return(otheryear)
    }
  })
  
  data <- reactive({
    if(input$year==0){
      year2010
    }else if(input$year==1){
      fileupload()
    }
  })
  
  p1<-reactive({
    
    if(input$year==0){
      data=year2010
    }else{
      data=fileupload()
    }
    
    if (input$Gender1 == 0){ 
      gender<-0
    }else{
      gender<-1
    }
    
    age<-input$Age1
    n<-input$n1  
    m<-input$m1
    rate<-input$rate1 *0.01
    b<-input$b1 * 10000
    
    p1<-comma(round(premium(data, gender, age,rate, n,m,b)$px,0),format='d')
    
  })
  
  
  output$p1 <- renderValueBox({
    valueBox(
      p1(), "Premium Price", icon = icon("won"),
      color = "aqua", width=12
    )})
  
  
  Bp<-reactive({
    
    if(input$year==0){
      data=year2010
    }else{
      data=fileupload()
    }
    
    
    if (input$Bgender == 0){
      Bp <- comma(round(calculate.px(data, 'f',input$Bage, input$Bi/100, input$Bn, input$Bm, input$Ba)*10000,0),format='d')
      
    }else if (input$Bgender == 1){
      Bp <- comma(round(calculate.px(data, 'm',input$Bage, input$Bi/100, input$Bn, input$Bm, input$Ba)*10000,0),format = 'd')
      
      
    }
  })
  
  output$Bp <- renderValueBox({
    valueBox(
      Bp(), "Premium Price", icon = icon("won"),
      color = "green", width=12
    )})
}


## app.R ##
shinyApp(ui, server)