rm(list=ls())
library(readxl)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(formattable)


#````````````````````````````````````````````````````````````````````````
car <- read_xls('C:/Users/SY/Desktop/2020-2/이론통계학2/Project6/data.xls')
  
colnames(car)[1] <- 'Kilometers'
car$Kilometers <- as.factor(car$Kilometers)
car$Zone <- as.factor(car$Zone)
car$Make <- as.factor(car$Make)
car$Bonus<-as.factor(car$Bonus)
car$mu <- car$Payment/car$Claims

data.x <- car[,c(1,2,3,4,8)]
data2.x <- car[,c(1,2,3,4,6,7)]
data3.x <- car[,c(1,2,3,4,5)]
data2 <- na.omit(car)

premium<-function(data, deduct, lim, bon, kilo, zz, make){
  
  glm1<-glm(Claims~ offset(log(Insured + 1)) + Kilometers + Zone + Bonus + 
              Make + Kilometers:Zone + Kilometers:Bonus + Kilometers:Make + 
              Zone:Bonus + Zone:Make + Bonus:Make, data=car, family="poisson"(link="log"))
  
  lm <- glm(Payment/Claims~Kilometers+Zone+Make+Bonus+Kilometers:Zone+Kilometers:Make+Kilometers:Bonus+Zone:Make+Zone:Bonus+Make:Bonus,
            data=car, family=Gamma(link='log'), weight=car$Claims)
  
  data <- data %>% mutate(poisson=predict(glm1, data, type='response'),
                          gamma=predict(lm, data, type='response'))
  
  A<-deduct
  B<-lim
  
  poisson.values <-data[data$Bonus==bon & data$Kilometers==kilo & data$Zone==zz & data$Make==make,]$poisson
  m <- data[data$Bonus==bon & data$Kilometers==kilo & data$Zone==zz & data$Make==make,]$Insured
  lnMu <- data[data$Bonus==bon & data$Kilometers==kilo & data$Zone==zz & data$Make==make,]$gamma
  
  lambda <- poisson.values/(1 + m)
  Mu <- min(max(lnMu-A, 0),B)
  
  prem <- lambda*Mu
  return(prem*127.4)
}


premium(read.csv('C:/Users/SY/Desktop/data.csv'), 10, 10, 1,1,1,1)

## ui.R ##

header <- dashboardHeader(title = "Car Insurance")
sidebar <- dashboardSidebar(
  sidebarMenu(
    fileInput('file1', label=h6('Choose CSV File'),
              accept=c('text/csv',  'text/comma-separated-values,text/plain', '.csv')),
    id = "tabs",
    
    numericInput('bonus', 'Bonus', value=2, min=1, max=7, step=1),
    
    #checkboxInput('no', 'No LIMIT and No Deductible', value = FALSE ),
    
    sliderInput('deductible',
                label = "Deductible",
                min = 0,
                max = 1000,
                value = 10),
    
    sliderInput('limit',
                label = "Limit",
                min = 1000,
                max = 100000,
                value = 10),
    
    menuItemOutput("slider_sidebar"),
    
    menuItemOutput("Text_input")
  )
)


## Body
body <- dashboardBody(
  fluidRow(
    
    box(title="Kilometers", status="warning", width=4, height=300, solidHeader=TRUE,
        radioButtons("km", "" ,
                     choices = list("1: less than 1000" = 1, 
                                    "2: from 1000 to 15000" = 2,
                                    "3: 15000 to 20000" = 3,
                                    "4: 20000 to 25000" = 4,
                                    "5: more than 25000" = 5), selected = 1)),
    
    box(title="Zone", status="warning", width=4, height=300, solidHeader=TRUE,
        radioButtons("zone", "" ,
                     choices = list("Seoul" = 1, 
                                    "Gyeonggi-do" = 2,
                                    "Gangwon-do" = 3,
                                    "Daegu" = 4,
                                    "Busan" = 5,
                                    "Chungcheongnam-do" = 6,
                                    "Chundcheongbuk-do" = 7), selected = 1)),
    
    box(title="Type of Cars", status="warning", width=4, height=300, solidHeader=TRUE,
        radioButtons("car", "" ,
                     choices = list("KIA-K7" = 1, 
                                    "KIA-K9" = 2,
                                    "KIA-Morning" = 3,
                                    "Hyundai-Granger" = 4,
                                    "Hyndai-Genesis" = 5,
                                    "Hyndai-Sonata" = 6,
                                    "Hyundai-Santafe" = 7,
                                    "Chevrolet-Spark" = 8,
                                    "Benz-Sedan" = 9), selected = 1))),
    fluidRow(
      valueBoxOutput("p1", width=12)
    )
)




ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  
  fileupload<- reactive({
    inFile <- input$file1
    if (is.null(inFile)){
      return(NULL)
    }else {
      new.data <- read.csv(inFile$datapath, stringsAsFactors = T)
      return(new.data)
    }
  })
  
  p1<-reactive({
    
    data <- fileupload()
    
    data$Kilometers <- as.factor(data$Kilometers)
    data$Zone <- as.factor(data$Zone)
    data$Make <- as.factor(data$Make)
    data$Bonus<-as.factor(data$Bonus)
    
    deduct <- as.numeric(input$deductible)
    lim <- as.numeric(input$limit)
    
    bon <- as.numeric(input$bonus)
    
    
    kilo <- as.numeric(input$km)
    zz <- as.numeric(input$zone)
    make <- as.numeric(input$car)
    
  
    p1<-comma(round(premium(data, deduct, lim, bon, kilo, zz, make), 0), format='d')
    return(p1)
  })
  
  
  
  output$p1 <- renderValueBox({
    valueBox(
      p1(), "Premium Price", icon = icon("won"),
      color = "aqua", width=12
    )})
  
}


## app.R ##
shinyApp(ui, server)