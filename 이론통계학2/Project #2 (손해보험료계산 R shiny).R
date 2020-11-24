rm(list=ls())
library(readxl)
library(tidyverse)
library(shiny)
library(shinydashboard)


table2<-read_excel("C:/Users/Yoonhee/Desktop/수업자료, 과제/대학원/2학기/이통2/hw2/상해보험자료2.xlsx", sheet=1)
table2<-table2[1:50,]
colnames(table2)<-c("X", "n")
table2$X<-as.numeric(table2$X)


lognormal <- function(A,B,data){
  X<-as.matrix(data[,1])
  Y<-log(sort(X*10000))
  r<-cumsum(data[,2])
  n= 8443
  m<-r/(n+1)
  p<-qnorm(m[,1])
  
  R2<-summary(lm(Y~p))$r.squared
  mu<-summary(lm(Y~p))$coef[1,1]
  sigma<-summary(lm(Y~p))$coef[2,1]
  
  
  lognormal_xpdf<-function(x){
    x*(1/(x*sqrt(2*pi)*sigma)*exp(-0.5*((log(x)-mu)/sigma)^2))
  }
  lognormal_cdf<-function(x){
    pnorm((log(x)-mu)/sigma)
  }
  lognormal_pdf<-function(x){
    (1/(x*sqrt(2*pi)*sigma)*exp(-0.5*((log(x)-mu)/sigma)^2))
  }
  
  EX<-integrate(lognormal_xpdf, A, A+B)$value+
    B*(1-lognormal_cdf(A+B))-
    A*(lognormal_cdf(A+B)-
         lognormal_cdf(A))
  
  p<-EX*(8443/271306)
  par<-list("p"=p,"R2"=R2,"mu"=mu,"sigma"=sigma, "qqplot"=ggplot)
  
  return(par)
}

pareto<- function(A, B, data){
  X<-as.matrix(data[,1])
  r<-cumsum(data[,2])
  n= 8443
  m<-r/(n+1)
  p<- -log(1-m[,1])
  
  paretox<-function(par, data){
    lambda<-par[1]
    Y<-log(1+sort(X*10000)/lambda)
    R2<-summary(lm(Y~p+0))$r.squared
    return(-R2)
  }
  result<-optim(par=c(0.1),data=data, fn=paretox, method="L-BFGS-B", lower=0, upper=1000000)
  
  lambda<-result$par[1]
  
  Y<-log(1+sort(X*10000)/lambda)
  R2<-summary(lm(Y~p+0))$r.squared
  alpha<- 1/summary(lm(Y~p+0))$coef[1,1]
  
  Pareto_xpdf<-function(x){
    x*(alpha*(lambda^alpha)*((lambda+x)^(-alpha-1)))
  }
  Pareto_cdf<-function(x){
    1-((lambda/(lambda+x))^alpha)
  }
  
  EN<-integrate(Pareto_xpdf, A, A+B)$value+B*(1-Pareto_cdf(A+B))-A*(Pareto_cdf(A+B)-Pareto_cdf(A))
  p<-EN*(8443/271306)
  
  par<-list("p"=p,"R2"=R2,"alpha"=alpha,"lambda"=lambda,"qqplot"=ggplot)
  return(par)  
}


weibull <- function(A, B, data){
  X<-as.matrix(data[,1])
  Y<-log(sort(X*10000))
  r<-cumsum(data[,2])
  n= 8443
  m<-1-r/(n+1)
  p<-log(-log(m[,1]))
  R2<-summary(lm(Y~p))$r.squared
  mu<-summary(lm(Y~p))$coef[1,1]
  sigma<-summary(lm(Y~p))$coef[2,1]
  
  tau<-1/sigma
  c<-exp(-mu/sigma)
  
  
  weibull_cdf<-function(x){
    1-exp(-c*(x^tau))
  }
  
  weibull_xpdf<-function(x){
    x* (c*tau*(x^(tau-1))*exp(-c*(x^tau)))
  }
  
  EN<-integrate(weibull_xpdf, A, A+B)$value+B*(1-weibull_cdf(A+B))-A*(weibull_cdf(A+B)-weibull_cdf(A))
  p<-EN*(8443/271306)
  
  par<-list("p"=p,"R2"=R2,"tau"=tau,"c"=c, "qqplot"=ggplot)
  return(par)
}


Frechet<-function(A, B, data){
  X<-as.matrix(data[,1])
  Y<-log(sort(X*10000))
  r<-cumsum(data[,2])
  n= 8443
  m<-r/(n+1)
  p<- -log(-log(m[,1]))
  R2<-summary(lm(Y~p))$r.squared
  mu<-summary(lm(Y~p))$coef[1,1]
  sigma<-summary(lm(Y~p))$coef[2,1]
  
  tau<-1/sigma
  c<-exp(mu/sigma)
  
  Frechet_xpdf<-function(x){
    x * (c*tau*exp(-c/(x^tau))/x^(tau+1))
  }
  Frechet_cdf<-function(x){
    exp(-c/(x^(tau)))
  }
  Frechet_pdf<-function(x){
    c*tau*exp(-c/(x^tau))/x^(tau+1)
  }
  
  EN<-integrate(Frechet_xpdf, A, A+B)$value+B*(1-Frechet_cdf(A+B))-A*(Frechet_cdf(A+B)-Frechet_cdf(A))
  p<-EN*(8443/271306)
  par<-list("p"=p,"R2"=R2,"tau"=tau,"c"=c, "qqplot"=ggplot)
  return(par)
}

## part3 ##
library(readxl)
fire <- read.csv('C:/Users/Yoonhee/Desktop/수업자료, 과제/대학원/2학기/이통2/hw2/2008-2017-학교화재.csv', header=T, sep=',')
library(tidyverse)
library(jmuOutlier)
fire_qq = fire[,c(1,7:11)] # 2013년~2017년
fire_qq = fire_qq[-7,] # 10억 이상 제거
colnames(fire_qq)=c('damage','2013','2014','2015','2016','2017')
fire_qq = fire_qq%>%mutate(freq=apply(fire_qq[,2:6],1,sum)) # freq
fire_qq = fire_qq%>%mutate(cumfr = as.vector(apply(t(fire_qq[['freq']]),1,cumsum))) # cumfreq
fire_qq = fire_qq%>%mutate(pr = as.vector(apply(t(fire_qq[['cumfr']]),1,function(x){x/(986+1)}))) # pr
fire_qq$damage=as.numeric(as.character(fire_qq$damage)) # 피해액 숫자로 변환
fire_qq$lnX = log(fire_qq$damage)

# log-normal
lognormal3 <- function(A, B, data){
  q <- qnorm(data$pr)
  Mu<-summary(lm(lnX~q, data=data))$coef[1,1]
  sigma<-summary(lm(lnX~q, data=data))$coef[2,1]
  
  lognormal_xpdf<-function(x){
    x*(1/(x*sqrt(2*pi)*sigma)*exp(-0.5*((log(x)-Mu)/sigma)^2))
  }
  lognormal_cdf<-function(x){
    pnorm((log(x)-Mu)/sigma)
  }
  lognormal_pdf<-function(x){
    (1/(x*sqrt(2*pi)*sigma)*exp(-0.5*((log(x)-Mu)/sigma)^2))
  }
  
  EX<-integrate(lognormal_xpdf, A, A+B)$value+
    B*(1-lognormal_cdf(A+B))-
    A*(lognormal_cdf(A+B)-
         lognormal_cdf(A))
  
  p<-986/(5*21162)
  premium<-EX*p
  
  return(c(premium))
}

# log-logistic
loglogistic3 <- function(A, B, data){
  x <- log(data$cumfr/(986+1-data$cumfr))
  Mu<-summary(lm(lnX~x, data=data))$coef[1,1]
  sigma<-summary(lm(lnX~x, data=data))$coef[2,1]
  
  alpha<- 1/sigma
  lambda<-exp(Mu)
  
  
  loglogistic_xpdf<-function(x){
    x*alpha*x^(alpha-1)*lambda^(-alpha)/(1+(x/lambda)^alpha)^2
  }
  loglogistic_cdf<-function(x){
    1-(1/(1+(x/lambda)^alpha))
  }
  loglogistic_pdf<-function(x){
    alpha*x^(alpha-1)*lambda^(-alpha)/(1+(x/lambda)^alpha)^2
  }
  
  EX<-integrate(loglogistic_xpdf, A, A+B)$value+
    B*(1-loglogistic_cdf(A+B))-
    A*(loglogistic_cdf(A+B)-
         loglogistic_cdf(A))
  
  p<-986/(5*21162)
  premium<-EX*p
  
  return(c(premium))
  
  
}

# log-gumbel(Frechet)
frechet3 <- function(A, B, data){
  x <- -log(-log(data$pr))
  Mu<-summary(lm(lnX~x, data=data))$coef[1,1]
  sigma<-summary(lm(lnX~x, data=data))$coef[2,1]
  
  tau<-1/sigma
  c<-exp(Mu/sigma)
  
  Frechet_xpdf<-function(x){
    x * (c*tau*exp(-c/(x^tau))/x^(tau+1))
  }
  Frechet_cdf<-function(x){
    exp(-c/(x^(tau)))
  }
  Frechet_pdf<-function(x){
    c*tau*exp(-c/(x^tau))/x^(tau+1)
  }
  
  EX<-integrate(Frechet_xpdf, A, A+B)$value+
    B*(1-Frechet_cdf(A+B))-
    A*(Frechet_cdf(A+B)-
         Frechet_cdf(A))
  
  p<-986/(5*21162)
  premium<-EX*p
  
  return(c(premium))
  
  
}
# log-exponential(Pareto)
pareto3<- function(A, B, data){
  pareto_lambda <- function(par,data){
    lambda <- par[1]
    y <- log(1+data$damage/lambda)
    x <- -log(1-data$pr)
    R2 <- summary(lm(y~x-1, data=data))$r.squared
    return(R2) 
  }
  
  opt1<-optim(par=c(100), data=data, pareto_lambda,control=list(fnscale=-1)) # 340570
  lambda<-opt1$par
  
  y <- log(1+data$damage/lambda)
  x <- -log(1-data$pr)
  sigma <- summary(lm(y~x-1, data=data))$coef[,1]
  
  alpha <- 1/sigma
  
  Pareto_xpdf<-function(x){
    x*(alpha*(lambda^alpha)*((lambda+x)^(-alpha-1)))
  }
  Pareto_cdf<-function(x){
    1-((lambda/(lambda+x))^alpha)
  }
  
  EX<-integrate(Pareto_xpdf, A, A+B)$value+
    B*(1-Pareto_cdf(A+B))-
    A*(Pareto_cdf(A+B)-
         Pareto_cdf(A))
  
  p<-986/(5*21162)
  premium<-EX*p
  
  return(c(premium))
  
}









## ui.R ##

header <- dashboardHeader(title = "Insurance")
sidebar <- dashboardSidebar(
  sidebarSearchForm(label = "Search...", "searchText", "searchButton"),
  sidebarMenu(
    id = "tabs",
    # Dashboard
    menuItem("Accident", tabName = "part1", icon = icon("hospital")),
    # Widgets
    menuItem("Fire", tabName = "part2", icon = icon("fire")),
    
    menuItemOutput("slider_sidebar"),
    
    menuItemOutput("Text_input")
  )
)

## Body
body <- dashboardBody(
  
  tabItems(
    tabItem("part1",  
            fluidRow(
              box(title="Deductible", status="primary",width=4,height=200,solidHeader = TRUE,
                  sliderInput("A1", "unit : 10000 won", 0, 20, 0))
              ,
              box(title="Limit", status="primary", width=4,height=200,solidHeader = TRUE, 
                  sliderInput("B1","unit : 10000 won", 50, 1000, 50))
              ,
              box(title = "Distribution1", width = 4, height=200,solidHeader = TRUE, status = "primary", 
                  radioButtons("distribution1","Claim Size Distribution",
                               c("pareto"=1,"Frechet"=2, "lognormal"=3,"weibull"=4)))
            ),
            fluidRow(
              valueBoxOutput("p1",width=12)
            ))
    
    
    
    ,tabItem("part2",
             fluidRow(
               box(title="Deductible", status="danger",width=4,height=220,solidHeader = TRUE,
                   sliderInput("A2", "unit : 10000 won", 0, 20, 0))
               ,
               box(title="Limit", status="danger", width=4,height=220,solidHeader = TRUE, 
                   sliderInput("B2","unit : 10000 won", 10000, 100000, 10000))
               ,
               box(title = "Distribution", width = 4, height=220,solidHeader = TRUE, status = "danger", 
                   radioButtons("distribution2","Claim Size Distribution",
                                c("pareto"=1,"Frechet"=2, "lognormal"=3,"loglogistic"=4)))
             ),
             fluidRow(
               valueBoxOutput("p2", width=12)
             )
    )))



ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  
  p1<-reactive({
    
    if (input$distribution1 == 1){ 
      round(pareto(input$A1*10000, input$B1*10000, table2)$p,0)
      
    }else if (input$distribution1 == 2){
      round(Frechet(input$A1*10000, input$B1*10000, table2)$p,0)
      
    }else if (input$distribution1 == 3){
      round(lognormal(input$A1*10000, input$B1*10000, table2)$p,0)
      
    }else if (input$distribution1 == 4){
      round(weibull(input$A1*10000, input$B2*10000, table2)$p,0)
      
    }
  })
  
  output$p1 <- renderValueBox({
    valueBox(
      p1(), "Premium Price", icon = icon("won"),
      color = "blue", width=12
    )})
  
  
  p2<-reactive({
    if (input$distribution2 == 1){
      round(pareto3(input$A2, input$B2, fire_qq)*10000,0)
      
    }else if (input$distribution2 == 2){
      round(frechet3(input$A2, input$B2, fire_qq)*10000,0)
      
    }else if (input$distribution2 == 3){
      round(lognormal3(input$A2, input$B2, fire_qq)*10000,0)
      
    }else if (input$distribution2 == 4){
      round(loglogistic3(input$A2, input$B2, fire_qq)*10000,0)
      
    }
  })
  
  output$p2 <- renderValueBox({
    valueBox(
      p2(), "Premium Price", icon = icon("won"),
      color = "red", width=12
    )})
}


## app.R ##
shinyApp(ui, server)