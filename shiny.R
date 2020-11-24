rm(list=ls())

library(readxl)
library(shiny)
library(shinydashboard)
library(survival)

setwd('C:/Users/Yoonhee/Desktop/수업자료, 과제/대학원/2학기/이통2/hw9')
data <- read_excel('data.xls')
data <- data[,-c(1,9)]
data <- na.omit(data)
attach(data)

#`````````````모델 학습```````````````#

#COX
cox.model <- coxph(Surv(followup, chdfate) ~ sbp + (1/bmi) + (dbp^2) + age + scl + 
                     sex + sbp:age + sbp:scl + (1/bmi):age + (dbp^2):age + (dbp^2):scl + 
                     age:sex, data=data)


#ALT
alt.weibull <- survreg(formula = Surv(followup, chdfate) ~ sbp + dbp + scl + age + bmi + sex + 
                         month + age:sex + sbp:age + dbp:age + sbp:dbp + age:bmi + 
                         sbp:bmi + dbp:bmi + scl:age + sbp:age:bmi + dbp:age:bmi + 
                         sbp:dbp:bmi + sbp:dbp:age + sbp:dbp:age:bmi, data=data, dist='weibull')



#`````````````확률 계산```````````````#

#COX
#followup = 1; chdfate =1; 
#sbp = 1; bmi =1; dbp = 1; age = 10; scl = 1; sex = 1;
cox.prob <- function(sbp, bmi, dbp, age, scl, sex){
  
  data5 <- data.frame(#followup = followup, chdfate = chdfate, 
    sbp = sbp,  bmi = bmi, dbp = dbp, age = age, scl = scl, sex = sex)
  
  S2 <- data.frame(time = survfit(cox.model)$time, surv = survfit(cox.model)$surv, 
                   hazard = -log(survfit(cox.model)$surv))
  lm_wei <- lm(log(hazard) ~ log(time), data = S2)
  alpha <- summary(lm_wei)$coefficients[1]
  beta <- summary(lm_wei)$coefficients[2]
  
  Ht_base <- exp(alpha+beta*log(3650))
  St_base <- exp(-Ht_base)
  
  cox.mu <- exp(predict(cox.model, newdata=data.frame(sbp = data5$sbp,
                                                      bmi= data5$bmi,
                                                      dbp=data5$dbp,
                                                      age=data5$age,
                                                      scl=data5$scl,
                                                      sex=data5$sex) ,type = 'lp'))
  
  prob.cox <- 1-St_base^cox.mu
  return(prob.cox)
}



#ALT
#sbp = 1; dbp = 1; scl = 1; age = 10; bmi = 15; sex = 1; month = 12;
alt.prob <- function(sbp, dbp, scl, age, bmi, sex, month){
  X <- matrix( c(1, sbp,  dbp,  scl,  age,  bmi, sex,  month,  age*month, sbp*age, 
                 dbp*age, sbp*dbp, age*bmi, sbp*bmi, dbp*bmi, scl*age, sbp*age*bmi, dbp*age*bmi, 
                 sbp*dbp*bmi, sbp*dbp*age, sbp*dbp*age*bmi), nrow = 1)
  
  mu <- (alt.weibull$coefficients) %*% t(X)
  sigma <- alt.weibull$scale
  z <- (log(3650)-mu)/sigma
  prob <- 1 - exp(-exp(z))
  return(as.numeric(prob))
}


##``````````````````````````````````````````````##
header <- dashboardHeader(title='Survival Analysis')
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('Survival Analysis', tabName = 'part3', icon = icon('won')),
    id = 'tabs'
  )
)


## Body
body <- dashboardBody(
  
  tabItems(
    tabItem('part3',
            
            fluidRow(
              box(title = 'sbp', status = 'primary', width = 3, height = 150, solidHeader = T,
                  numericInput('sbp','',value = 100, min = 0, max = 300, step = 1)),
              box(title = 'dbp', status = 'primary', width = 3, height = 150, solidHeader = T,
                  numericInput('dbp','',value = 65, min = 0, max = 150, step = 1)),
              box(title = 'age',status = 'primary', width = 3, height = 150, solidHeader = T, 
                  numericInput('age','', value = 60, min = 0, max = 100, step = 1)),
              box(title = 'scl',status = 'primary', width = 3, height = 150, solidHeader = T,
                  numericInput('scl','', value = 190, min = 100, max = 600, step = 1))
            ),
            
            fluidRow(
              box(title = 'bmi', status = 'primary', width = 3, height = 150, solidHeader = T,
                  numericInput('bmi','', value = 25, min = 15, max = 60, step = 0.1)),
              box(title = 'sex',status = 'primary',width = 3, heigth = 150, solidHeader = T,
                  radioButtons('sex','', selected = 0, choices = c('Female'=0, 'Male'=1))),
              box(title = 'month',status = 'primary', width = 3, height = 150, solidHeader = T,
                  numericInput('month', '', value = 1, min = 1, max = 12, step = 1)),
              box(title = 'model', status = 'primary', width = 3, height = 150, solidHeader = T,
                  radioButtons('model','', selected = 1, choices = c('COX'=1, 'ALT'=2)))
            ),
            
            fluidRow(
              valueBoxOutput('p', width = 12)
            )
    )))

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session){
  
  # COX 확률 계산 함수
  cox <- reactive({
    cox_p <- cox.prob(as.numeric(input$sbp), as.numeric(input$bmi), as.numeric(input$dbp), 
                      as.numeric(input$age), as.numeric(input$scl), as.numeric(input$sex))
    return(cox_p)
  })
  
  
  # ALT 확률 계산 함수
  alt <- reactive({
    alt_p <- alt.prob(as.numeric(input$sbp), as.numeric(input$dbp), as.numeric(input$scl), 
                      as.numeric(input$age), as.numeric(input$bmi), as.numeric(input$sex), 
                      as.numeric(input$month))
    return(alt_p)
  })
  
  
  # 최종 확률
  result <-reactive({
    if(input$model == 1){
      return(cox())
    }else{
      return(alt())
    }
  })
  
  output$p <- renderValueBox({
    valueBox(round(result(), 3), 'Probability', color = 'olive', width = 12)
  })
}


shinyApp(ui, server)