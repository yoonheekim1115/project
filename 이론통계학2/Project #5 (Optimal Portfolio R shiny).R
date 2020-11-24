rm(list=ls())
library(dplyr)
library(timetk)
library(Rsolnp)
library(ggplot2)
library(gridExtra)
library(shiny)
library(shinydashboard)
library(formattable)
setwd('C:/Users/Yoonhee/Desktop/수업자료, 과제/대학원/2학기/이통2/hw5')


##``````````````
#data <- read.csv('data.csv',header=T, sep=',')
shiny<-function(data,UB, LB, i_star, M, V0){
  train<-data[1:60,]
  test<-data[61:120,]
  
  i_star12<-(1+i_star)^(1/12)-1
  
  optim_theta<-function(par){
    theta0<-par[1];theta1<-par[2];theta2<-par[3];theta3<-par[4];theta4<-par[5];theta5<-par[6]
    rpt<-train[,1]*theta0 + train[,2]*theta1 + train[,3]*theta2 + train[,4]*theta3 + train[,5]*theta4+
      train[,6]*theta5
    rp_bar<-mean(rpt)
    sp<-sd(rpt)
    Sharpe<-(rp_bar-i_star12)/sp
    return(as.numeric(-Sharpe))
  }
  
  equal <- function(x) {
    x[1] + x[2] + x[3] + x[4]+ x[5]+x[6]
  }
  
  result_np<-solnp(c(rep(0.1,6)),
                   optim_theta,eqfun=equal,eqB=1,LB=LB,UB=UB, control=list(trace=FALSE))
  
  par<-round(result_np$pars,4)
  
  theta0<-par[1];theta1<-par[2];theta2<-par[3];theta3<-par[4];theta4<-par[5];theta5<-par[6]
  rpt<-train[,1]*theta0 + train[,2]*theta1 + train[,3]*theta2 + train[,4]*theta3 + train[,5]*theta4+
    train[,6]*theta5
  rp_bar<-mean(rpt)
  sp<-sd(rpt)
  Sharpe<-(rp_bar-i_star12)/sp
  
  
  rps<-test[,1]*theta0 + test[,2]*theta1 + test[,3]*theta2 + test[,4]*theta3 + test[,5]*theta4+
    test[,6]*theta5
  rp_star<-mean(rps)
  sp_star<-sd(rps)
  
  validation<-matrix(0,2,2)
  validation[1,1]<-round(rp_bar*12,3) ; validation[1,2]<-round(rp_star*12,3) ; 
  validation[2,1]<-round(sp*sqrt(12),3) ;  validation[2,2]<-round(sp_star*sqrt(12),3) ;  

  
  m<-12*M
  
  Vt<-as.data.frame(matrix(0,m,3))
  Vt$rpt<-V0*cumprod(1+rpt)[1:m]
  Vt$rps<-V0*cumprod(1+rps)[1:m]
  Vt$t<-seq(1:m)
  
  plot<-ggplot(data=Vt, aes(x=t, y=rpt), color="blue")+
    geom_line()+
    geom_line(aes(x=t, y=rps), color="red")+
    ggtitle("optimized portfolio's value", subtitle="red=test / black=train")+
    ylab("")
  
  plot2<-ggplot(data=Vt, aes(x=t, y=rpt), color="blue")+
    geom_line()+
    ggtitle("optimized portfolio's value")+
    ylab("")
  
  result=list("theta"=par, "mean"=rp_bar, "sp"=sp, "Sharpe_ratio"=Sharpe,"validation"=validation,"plot"=plot, "plot2"=plot2)
  return(result)
}

#LB<-c(0,0,0,0,0,0)
#UB<-c(1,1,1,1,1,1)
#shiny(data, UB, LB, 0.12, 5, 10000000)


##``````````````

header <- dashboardHeader(title = "Optimal Portfolio")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Optimal Portfolio", tabName = "part3", icon = icon("won")),
    
    fileInput('file1', label=h6('Choose CSV File'),
              accept=c('text/csv',  'text/comma-separated-values,text/plain', '.csv')),
    
    id = "tabs",
    
    numericInput('V0', 'Initial Investment Amount',value=10000000,0,100000000,10000),
    
    numericInput('i_star', 'Target Rate of Return(year average)',value=0.12,0,1,0.01),
    
    numericInput('M', 'Investment Period(year)',value=5,0,100,1),
    
    numericInput('N', 'Sample Data Size(year)',value=5,0,100,1),
    
    menuItemOutput("slider_sidebar"),
    
    menuItemOutput("Text_input")
  )
)

## Body
body <- dashboardBody(
  
  tabItems(
    tabItem("part3",
            fluidRow(
              box(title='deposit',status='warning',width=4, height=150, solidHeader = T,
                  sliderInput('theta1','',0,1,c(0,1))),
              box(title='Samsung Electronics',status='warning',width=4, height=150, solidHeader = T,
                  sliderInput('theta2','',0,1,c(0,1))),
              box(title='Kakao',status='warning',width=4, height=150, solidHeader = T,
                  sliderInput('theta3','',0,1,c(0,1)))
            ),
            fluidRow(
              box(title='KT&G',status='warning',width=4, height=150, solidHeader = T,
                  sliderInput('theta4','',0,1,c(0,1))),
              box(title='Korea Gas Corp.',status='warning',width=4, height=150, solidHeader = T,
                  sliderInput('theta5','',0,1,c(0,1))),
              box(title='Naver',status='warning',width=4, height=150, solidHeader = T,
                  sliderInput('theta6','',0,1,c(0,1)))
            ),
            fluidRow(
              valueBoxOutput("mean",width=4),
              valueBoxOutput("sd",width=4),
              valueBoxOutput("sharperatio",width=4)
            ),
            fluidRow(
              box(title='theta', status = 'primary',tableOutput('theta'))
            ),
            fluidRow(
              valueBoxOutput('aem',width = 3),
              valueBoxOutput('aes',width = 3),
              valueBoxOutput('arm',width = 3),
              valueBoxOutput('ars',width = 3)
            ),
            fluidRow(
              box(title='plot', status = 'primary', width=12, plotOutput('plot'))
            )
    )))

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  
  fileupload<- reactive({
    inFile <- input$file1
    if (is.null(inFile)){
      return(NULL)
    }else {
      traindata <- read.csv(inFile$datapath, header = TRUE)
      return(traindata)
    }
  })


  mean<-reactive({
    inFile <- input$file1
    if(is.null(inFile)){
      traindata=NULL
    }else{
      traindata<-fileupload() 
      }
    LB<-c(input$theta1[1],input$theta2[1],input$theta3[1],input$theta4[1],input$theta5[1], input$theta6[1])
    UB<-c(input$theta1[2],input$theta2[2],input$theta3[2],input$theta4[2],input$theta5[2], input$theta6[2])
    i_star<-input$i_star
    M<-input$M
    V0<-input$V0
    return(round(shiny(traindata,UB,LB,i_star,M,V0)$mean,3))
  })
  
  sd<-reactive({
    inFile <- input$file1
    if(is.null(inFile)){
      traindata=NULL
    }else{
      traindata<-fileupload() 
    }
    LB<-c(input$theta1[1],input$theta2[1],input$theta3[1],input$theta4[1],input$theta5[1], input$theta6[1])
    UB<-c(input$theta1[2],input$theta2[2],input$theta3[2],input$theta4[2],input$theta5[2], input$theta6[2])
    i_star<-input$i_star
    M<-input$M
    V0<-input$V0
    return(round(shiny(traindata,UB,LB,i_star,M,V0)$sp,3))
  })
  
  sharperatio<-reactive({
    inFile <- input$file1
    if(is.null(inFile)){
      traindata=NULL
    }else{
      traindata<-fileupload() 
    }
    LB<-c(input$theta1[1],input$theta2[1],input$theta3[1],input$theta4[1],input$theta5[1], input$theta6[1])
    UB<-c(input$theta1[2],input$theta2[2],input$theta3[2],input$theta4[2],input$theta5[2], input$theta6[2])
    i_star<-input$i_star
    M<-input$M
    V0<-input$V0
    return(round(shiny(traindata,UB,LB,i_star,M,V0)$Sharpe_ratio,3))
  })
  
  theta<-reactive({
    inFile <- input$file1
    if(is.null(inFile)){
      traindata=NULL
    }else{
      traindata<-fileupload() 
    }
    LB<-c(input$theta1[1],input$theta2[1],input$theta3[1],input$theta4[1],input$theta5[1], input$theta6[1])
    UB<-c(input$theta1[2],input$theta2[2],input$theta3[2],input$theta4[2],input$theta5[2], input$theta6[2])
    i_star<-input$i_star
    M<-input$M
    V0<-input$V0
    return(shiny(traindata,UB,LB,i_star,M,V0)$theta)
  })
  
  aem<-reactive({
    inFile <- input$file1
    if(is.null(inFile)){
      traindata=NULL
    }else{
      traindata<-fileupload() 
    }
    LB<-c(input$theta1[1],input$theta2[1],input$theta3[1],input$theta4[1],input$theta5[1], input$theta6[1])
    UB<-c(input$theta1[2],input$theta2[2],input$theta3[2],input$theta4[2],input$theta5[2], input$theta6[2])
    i_star<-input$i_star
    M<-input$M
    V0<-input$V0
    return(shiny(traindata,UB,LB,i_star,M,V0)$validation[1,1])
  })
  
  aes<-reactive({
    inFile <- input$file1
    if(is.null(inFile)){
      traindata=NULL
    }else{
      traindata<-fileupload() 
    }
    LB<-c(input$theta1[1],input$theta2[1],input$theta3[1],input$theta4[1],input$theta5[1], input$theta6[1])
    UB<-c(input$theta1[2],input$theta2[2],input$theta3[2],input$theta4[2],input$theta5[2], input$theta6[2])
    i_star<-input$i_star
    M<-input$M
    V0<-input$V0
    return(shiny(traindata,UB,LB,i_star,M,V0)$validation[2,1])
  })
  
  arm<-reactive({
    inFile <- input$file1
    if(is.null(inFile)){
      traindata=NULL
    }else{
      traindata<-fileupload() 
    }
    LB<-c(input$theta1[1],input$theta2[1],input$theta3[1],input$theta4[1],input$theta5[1], input$theta6[1])
    UB<-c(input$theta1[2],input$theta2[2],input$theta3[2],input$theta4[2],input$theta5[2], input$theta6[2])
    i_star<-input$i_star
    M<-input$M
    V0<-input$V0
    return(shiny(traindata,UB,LB,i_star,M,V0)$validation[1,2])
  })
  
  ars<-reactive({
    inFile <- input$file1
    if(is.null(inFile)){
      traindata=NULL
    }else{
      traindata<-fileupload() 
    }
    LB<-c(input$theta1[1],input$theta2[1],input$theta3[1],input$theta4[1],input$theta5[1], input$theta6[1])
    UB<-c(input$theta1[2],input$theta2[2],input$theta3[2],input$theta4[2],input$theta5[2], input$theta6[2])
    i_star<-input$i_star
    M<-input$M
    V0<-input$V0
    return(shiny(traindata,UB,LB,i_star,M,V0)$validation[2,2])
  })
  
  plot<-reactive({
    inFile <- input$file1
    if(is.null(inFile)){
      traindata=NULL
    }else{
      traindata<-fileupload() 
    }
    LB<-c(input$theta1[1],input$theta2[1],input$theta3[1],input$theta4[1],input$theta5[1], input$theta6[1])
    UB<-c(input$theta1[2],input$theta2[2],input$theta3[2],input$theta4[2],input$theta5[2], input$theta6[2])
    i_star<-input$i_star
    M<-input$M
    V0<-input$V0
    return(shiny(traindata,UB,LB,i_star,M,V0)$plot2)
  })
  
  output$mean <- renderValueBox({
    valueBox(mean(), "Mean", color = "green", width=4)})
  
  output$sd <- renderValueBox({
    valueBox(sd(), "Standard Deviation",color = "green", width=4)})
  
  output$sharperatio <- renderValueBox({
    valueBox(sharperatio(), "Sharpe Ratio",color = "green", width=4)})
  
  output$theta <- renderTable({
    inFile <- input$file1
    if(is.null(inFile)){
      traindata=NULL
    }else{
      traindata<-fileupload() 
    }
    LB<-c(input$theta1[1],input$theta2[1],input$theta3[1],input$theta4[1],input$theta5[1], input$theta6[1])
    UB<-c(input$theta1[2],input$theta2[2],input$theta3[2],input$theta4[2],input$theta5[2], input$theta6[2])
    i_star<-input$i_star
    M<-input$M
    V0<-input$V0
    thetatable = as.data.frame(matrix(0,nrow=1, ncol=6))
    thetatable[1,1] <- shiny(traindata,UB,LB,i_star,M,V0)$theta[1]
    thetatable[1,2] <- shiny(traindata,UB,LB,i_star,M,V0)$theta[2]
    thetatable[1,3] <- shiny(traindata,UB,LB,i_star,M,V0)$theta[3]
    thetatable[1,4] <- shiny(traindata,UB,LB,i_star,M,V0)$theta[4]
    thetatable[1,5] <- shiny(traindata,UB,LB,i_star,M,V0)$theta[5]
    thetatable[1,6] <- shiny(traindata,UB,LB,i_star,M,V0)$theta[6]
    colnames(thetatable) <- c('Term Deposit','SE','KK','KT','KG','NV')
    return(head(thetatable))})
  
  output$aem <- renderValueBox({
    valueBox(aem(), "Annualized Expected Mean",color = "aqua", width=6)})
  
  output$arm <- renderValueBox({
    valueBox(arm(), "Annualized Realized Mean",color = "red", width=6)})

  output$aes <- renderValueBox({
    valueBox(aes(), "Annualized Expected Sd",color = "aqua", width=6)})
  
  output$ars <- renderValueBox({
    valueBox(ars(), "Annualized Realized Sd",color = "red", width=6)})
  
  output$plot <- renderPlot({
    plot <- plot()
    grid.arrange(plot, nrow=1, ncol=1)})
}


shinyApp(ui, server)