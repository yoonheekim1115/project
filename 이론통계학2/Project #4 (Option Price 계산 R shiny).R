rm(list=ls())
library(shiny)
library(shinydashboard)
library(formattable)

samsung17<-read.csv("C:/Users/Yoonhee/Desktop/수업자료, 과제/대학원/2학기/이통2/hw4/sag 2017.01-2017.12.csv",header=TRUE,sep=",")
samsung16<-read.csv("C:/Users/userYoonhee/Desktop/수업자료, 과제/대학원/2학기/이통2/hw4/s016.01-2016.12.csv",header=TRUE,sep=",")

Asian_option<-function(data1, data2, rate, date, K, t){
  data1<-data1[c(1:2,6)]
  data2<-data2[c(1:2,6)]
  colnames(data1)<-c("date","end", "start")
  colnames(data2)<-c("date","end", "start")
  
  data1$start<-as.numeric(gsub(",","",data1$start))
  data1$end<-as.numeric(gsub(",","",data1$end))
  
  data2$start<-as.numeric(gsub(",","",data2$start))
  data2$end<-as.numeric(gsub(",","",data2$end))
  
  r<-rate
  dt<-1/nrow(data1)
  
  n<-nrow(data1)
  u_star<-diff(log(data1$end))
  u_star_bar<-sum(u_star)/n

  sigma<-sqrt(sum((u_star-u_star_bar)^2)/(n-1))/sqrt(dt)
  
  dt=1/250
  T<-250*t
  
  M<-1000 ;   C=c() ;   P=c() ;  St<-c() ;   St_bar<-c()
  
  St[1]<-data2$start[which(data2$date == date)]
                 
  for (i in 1:M){
    set.seed(i)
    Z<-rnorm(T)
    
    for(j in 2:(T+1)){
      St[j]<-St[j-1]*exp((r-(sigma^2/2))*dt+sigma*sqrt(dt)*Z[j-1])
    }
    
    St_bar=mean(St[2:(T+1)])
    C[i]=exp(-r*t)*max(St_bar-K, 0)
    P[i]=exp(-r*t)*max(K-St_bar,0)
  }
  
  ct<- sum(C)/M
  pt<- sum(P)/M
  
  result<-list("ct"=ct, "pt"=pt)
  return(result)
}

Asian_option(samsung16, samsung17, 0.05, "2017-01-02", 1770000, 1)
Asian_option(samsung16, samsung17, 0.05, "2017-01-02", 1770000, 1)

###``````````````````````````````````````````````````````````````````



header <- dashboardHeader(title = "Samsung Option Pricing")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Option pricing", tabName = "part1", icon = icon("won")),
    
    dateInput("date1", "Date", value="2017-01-02",min="2017-01-01", max="2018-12-31", format="yyyy-mm-dd"),
    
    fileInput('file1', label=h6('Choose CSV File'),
              accept=c('text/csv',  'text/comma-separated-values,text/plain', '.csv')),
    id = "tabs",
    
    menuItemOutput("slider_sidebar"),
    
    menuItemOutput("Text_input"),
    
    
  )
)


## Body
body <- dashboardBody(
  
  tabItems(
    tabItem("part1",  
            fluidRow(
              valueBoxOutput("St",width=6),
              valueBoxOutput("p1",width=6)
              ),
            fluidRow(
              box(title="K", status="warning", width=3,height=150,solidHeader=TRUE,
                  sliderInput("K1","(10000won)", 0, 500, 199)),
              box(title="put/call option", status="warning", width=3,height=150,solidHeader=TRUE,
                  radioButtons("put_call", "Select", 
                               choices = list("call option" = 0, "put option" = 1), selected = 0)),
              box(title="non-risky rate", status="warning", width=3,height=150,solidHeader=TRUE,
                  sliderInput("rate1","(%)", 0, 20, 5)),
              box(title="t", status="warning", width=3,height=150,solidHeader=TRUE,
                  sliderInput("t1", "(year)", 0, 20, 2))),
                        
    )))


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
  

  p1<-reactive({
    inFile <- input$file1
    if(is.null(inFile)){
      data2=samsung17
      data1=samsung16
    }else{
      data2=fileupload()
      data1=samsung17 }
    
    if (input$put_call==0){
      p1<-comma(round(Asian_option(data1, data2, input$rate1*0.01, input$date1 ,input$K1*10000,input$t1)$ct,
                      0),format='d')
    }else {
      p1<-comma(round(Asian_option(data1, data2, input$rate1*0.01, input$date1 ,input$K1*10000,input$t1)$pt,
                0),format='d')
    }
    return(p1)
  })
  
  St<-reactive({
    inFile <- input$file1
    if(is.null(inFile)){
      data2=samsung17
      data1=samsung16
    }else{
      data2=fileupload()
      data1=samsung17 }
    data1<-data1[c(1:2,6)]
    data2<-data2[c(1:2,6)]
    colnames(data1)<-c("date","end", "start")
    colnames(data2)<-c("date","end", "start")
    
    St<-data2$start[which(data2$date==input$date1)]
  })
  
  output$St <- renderValueBox({
      valueBox(
      St(), "Current price", icon = icon("won"),
      color = "green", width=4)})
  
  output$p1 <- renderValueBox({
    valueBox(
      p1(), "Option Price", icon = icon("won"),
      color = "aqua", width=4)})
  
 
}

shinyApp(ui, server)
