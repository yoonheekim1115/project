rm(list=ls())

library(readxl)
library(dplyr)
library(tidyverse)
library(readxl)
library(lubridate)
require(mgcv)
require(ggplot2)
library(ggpubr)
require(egg)
require(moonBook)
require(ztable)
library(shiny)
library(shinydashboard)
library(fastDummies)
library(e1071)
library(randomForest)
library(nnet)
library(pROC)
library(survival)
library(jtools)
library(xgboost)
library(class)
library(MASS)


data<-read_excel('C:/Users/Yoonhee/Desktop/수업자료, 과제/대학원/2학기/이통2/hw8/data.xls', sheet=1)
colnum<-c(1:(ncol(data)-3))
colnames<-gsub(" ", "", paste("x",colnum))
colnames<-c("ID", colnames, "y","calendarTime")
colnames(data)<- colnames
#`````````````````데이터 정리 `````````````````````````````

### 일반 데이터

#데이터 전처리 함수
preprocessing <- function(data){
  data[data==9999.99|data==-9999.99] <- NA  
  
  colSums(is.na(data))
  #### 윤희언니 ####
  nums <- unlist(lapply(data, is.numeric))  
  data.num <- data[ , nums]
  
  #부채비율
  #x4
  idx4 <- which(is.na(data$x4))
  lm4 <- lm(data$x4~data$x7+data$x13,na.action=na.omit)
  data$x4[idx4]<-mean(na.omit(data$x4))
  
  #유동부채비율
  #x7
  idx7 <- which(is.na(data$x7))
  lm7 <- lm(data$x7~data$x4+data$x13,na.action=na.omit)
  data$x7[idx7]<-mean(na.omit(data$x7))
  
  ###x13 ## x4, x7 뒤로 가야됨
  na_ind<-which(is.na(data$x13))
  fit<-lm(x13~x4+x7, data=data)
  data$x13[na_ind]<-mean(na.omit(data$x13))
  
  
  
  #총자산투자효율
  #x21
  idx21 <- which(is.na(data$x21))
  #which(abs(correlation[,22])>=0.6) ###correlation이 높은게 없음
  data$x21[idx21] <- mean(na.omit(data$x21))
  
  
  
  #매출채권증가율
  #x22
  idx22 <- which(is.na(data$x22))
  #which(abs(correlation[,23])>=0.6) ###correlation이 높은게 없음
  data$x22[idx22]<-mean(na.omit(data$x22))
  #which(is.na(data$x22))
  
  
  #재고자산증가율
  #x23
  idx23 <- which(is.na(data$x23))
  #which(abs(correlation[,24])>=0.6) ###correlation이 높은게 없음
  data$x23[idx23]<-mean(na.omit(data$x23))
  #which(is.na(data$x23))
  
  
  #경영자본순이익율
  #x24
  idx24 <- which(is.na(data$x24))
  #which(abs(correlation[,25])>=0.7)
  
  lm24 <- lm(data$x24~data$x28+data$x29+data$x30+data$x31,data=data, na.action=na.omit)
  data$x24[idx24]= lm24$fitted.values[idx24]
  
  
  
  #which(is.na(data$x24))
  
  #### 소진언니 ####
  # 재무비율로 원래 자료값 추정
  s1 <- exp(data$x42) # 총자산 = exp(로그자산)
  s2 <- s1*data$x5/100 # 총부채 = 총자산*(부차총계/자산총계비율)
  s3 <- s1-s2 # 총 자기자본 = 총자산 - 총부채
  s4 <- s3*data$x26/100 # 순이익 = 총자기자본*자기자본순이익율/100
  s5 <- s3*data$x27*100 # 자본금 = 총자기자본*자본금순이익율/100
  s6 <- s2*data$x25/100 # 금융비용 = 총부채*(금융비용/총부채비율)/100
  
  num<-c('x1','x2','x3','x4','x5','x6','x7','x8',
         'x9','x10','x11','x12','x13','x14','x15','x16',
         'x17','x18','x19','x20','x21','x22','x23','x24',
         'x25','x26','x27','x28','x29','x30','x31','x32',
         'x33','x34','x35','x36','x37','x38','x39','x40',
         'x41','x42')
  tmp1<-cor(data[,num], use="complete.obs")
  
  tmp1[20,][abs(tmp1[20,])>0.7] #
  tmp1[30,][abs(tmp1[30,])>0.7] # x30 ~ x24 + x28 + x29 + x31
  tmp1[31,][abs(tmp1[31,])>0.7] # x31 ~ x24 + x28 + x29 + x30 
  tmp1[32,][abs(tmp1[32,])>0.7] # x32 ~ x39 
  tmp1[33,][abs(tmp1[33,])>0.7] # 
  tmp1[34,][abs(tmp1[34,])>0.7] #
  tmp1[35,][abs(tmp1[35,])>0.7] #
  
  # x20
  na_ind <- which(is.na(data$x20)) 
  data$x20[na_ind]<-mean(data[-na_ind,]$x20)
  
  # x30
  na_ind <- which(is.na(data$x30)) 
  # 169  574  747 1257 1327 2643 2698 2876 2892 3745 3782 4098 4299 4575 4627 4952 5610 5642   5926 5946 6029 6204
  fit<-lm(x30 ~ x24 + x28 + x29 + x31, data=data)$fitted.value
  data$x30[na_ind]<-fit[na_ind]
  
  # x31
  na_ind <- which(is.na(data$x31)) 
  fit<-lm(x31 ~ x24 + x28 + x29 + x30, data=data)$fitted.value
  data$x31[na_ind]<-fit[na_ind]
  
  # x32
  na_ind <- which(is.na(data$x32)) 
  fit<-lm(x32 ~ x39, data=data)$fitted.value
  data$x32[na_ind]<-fit[na_ind]
  
  # x33
  na_ind <- which(is.na(data$x33)) 
  data$x33[na_ind]<-mean(data[-na_ind,]$x33)
  
  # x34
  na_ind <- which(is.na(data$x34)) 
  data$x34[na_ind]<-mean(data[-na_ind,]$x34)
  
  # x35
  na_ind <- which(is.na(data$x35)) 
  data$x35[na_ind]<-mean(data[-na_ind,]$x35)  
  
  #### 서영 ####
  reg.x16 <- lm(x16 ~ x12, data)
  data$x16[is.na(data$x16)] <- predict(reg.x16, newdata=data)[is.na(data$x16)]
  data$x16[is.na(data$x16)] <- mean(data$x16, na.rm=TRUE)
  
  
  reg.x17 <- lm(x17 ~ x12, data)
  data$x17[is.na(data$x17)] <- predict(reg.x17, newdata=data)[is.na(data$x17)]
  data$x17[is.na(data$x17)] <- mean(data$x17, na.rm=TRUE)
  
  debt <- data$x5 * exp(data$x42) # 부채
  equity <- exp(data$x42) - debt # 자기자본
  
  # 자기자본회전율
  data$x36[is.na(data$x36)] <- (exp(data$x41) / equity * 100)[is.na(data$x36)] # 자기자본회전율 = 매출/자기자본*100
  data$x36[1:1369][is.na(data$x36[1:1369])] <- mean(data$x36[1:1368], na.rm=TRUE) # train set
  data$x36[-c(1:1369)][is.na(data$x36[-c(1:1369)])] <- mean(data$x36[-c(1:1368)], na.rm=TRUE) # test set
  
  # 자본금회전율
  data$x37[1:1369][is.na(data$x37[1:1369])] <- mean(data$x37[1:1368], na.rm=TRUE) # train set
  data$x37[-c(1:1369)][is.na(data$x37[-c(1:1369)])] <- mean(data$x37[-c(1:1368)], na.rm=TRUE) # test set
  
  # 재고자산회전율
  data$x38[1:1369][is.na(data$x38[1:1369])] <- mean(data$x38[1:1368], na.rm=TRUE) # train set
  data$x38[-c(1:1369)][is.na(data$x38[-c(1:1369)])] <- mean(data$x38[-c(1:1368)], na.rm=TRUE) # test set
  
  # 총자본회전율
  data$x39[is.na(data$x39)] <- (exp(data$x41) / exp(data$x42) * 100)[is.na(data$x39)] # 총자본회전율 = 매출/총자산
  data$x39[1:1369][is.na(data$x39[1:1369])] <- mean(data$x39[1:1368], na.rm=TRUE) # train set
  data$x39[-c(1:1369)][is.na(data$x39[-c(1:1369)])] <- mean(data$x39[-c(1:1368)], na.rm=TRUE) # test set
  
  # 기업나이
  data$x40[1:1369][is.na(data$x40[1:1369])] <- mean(data$x40[1:1368], na.rm=TRUE) # train set
  data$x40[-c(1:1369)][is.na(data$x40[-c(1:1369)])] <- mean(data$x40[-c(1:1368)], na.rm=TRUE) # test set
  
  
  #### 보민 ####
  # 금융비용/총부채비율 x25
  # 금융비용/총부채비율 = s6/s2  ### s6[na_ind] NA값임
  # which(abs(correlation[,26])>=0.7)  ### correlation 높은게 없음
  # summary(data$x25)  ### mean과 median 비슷하므로 mean으로 결측치 채움
  na_ind <- which(is.na(data$x25))
  data$x25[na_ind]<-mean(na.omit(data$x25))
  
  # 자기자본순이익율 x26
  # 자기자본순이익율 = 순이익/자기자본 = s4/s3  ### s4[na_ind] NA값임
  # which(abs(correlation[,27])>=0.6) ### x26 ~ x3 + x36
  # summary(data$x26)  ### -9999.99 존재
  na_ind <- which(is.na(data$x26))
  fit <- lm(x26 ~ x3 + x36, data=data)$fitted.value
  data$x26[na_ind] <- fit[na_ind]
  # 설명변수가 NA인 경우
  na_ind <- which(is.na(data$x26))
  data$x26[na_ind]<-mean(na.omit(data$x26))
  
  # 자본금순이익율 x27
  # 자본금순이익율 = 순이익/자본금 = s4/s5  ### s4[na_ind] , s5[na_ind] NA값임
  # which(abs(correlation[,28])>=0.7)  ### correlation 높은게 없음
  # summary(data$x27)  ### -9999.99 존재
  na_ind <- which(is.na(data$x27))
  data$x27[na_ind]<-mean(na.omit(data$x27))
  
  # 총자본경상이익율 x28
  # 총자본경상이익율 = 경상이익/총 자본  ### 경상이익 구할 수 없음
  # which(abs(correlation[,29])>=0.7)  ### x28 ~ x24 + x29 + x30 + x31
  na_ind <- which(is.na(data$x28))
  fit <- lm(x28 ~ x24 + x29 + x30 + x31, data=data)$fitted.value
  data$x28[na_ind] <- fit[na_ind]
  
  # 총자본순이익율 x29
  # 총자본본순이익율 = 순이익/총 자본
  # which(abs(correlation[,30])>=0.7)  ### x29 ~ x24 + x28+ x30 + x31
  na_ind <- which(is.na(data$x29))
  fit <- lm(x29 ~ x24 + x28 + x30 + x31, data=data)$fitted.value
  data$x29[na_ind] <- fit[na_ind]
  
  # 유동비율 x8
  # 유동비율 = 유동자산/유동부채
  # which(abs(correlation[,9])>=0.7)  ### x8 ~ x20
  na_ind <- which(is.na(data$x8))
  fit <- lm(x8 ~ x20, data=data)$fitted.value
  data$x8[na_ind] <- fit[na_ind]
  
  # 고정재무비보상배율 x14
  # 고정재무비보상비율=(법인세비용차감전순이익+이자비용+고정재무비용)/(이자비용+고정재무비용)
  # which(abs(correlation[,15])>=0.7)  ### correlation 높은게 없음
  na_ind <- which(is.na(data$x14))
  data$x14[na_ind]<-mean(na.omit(data$x14))
  
  # 채연
  s1 <- exp(data$x42) # 총자산 = exp(로그자산)
  s2 <- s1*data$x5/100 # 총부채 = 총자산*(부차총계/자산총계비율)
  s3 <- s1-s2 # 총 자기자본 = 총자산 - 총부채
  
  ####x12  ## x16 뒤로 가야됨
  na_ind<-which(is.na(data$x12))
  fit<-lm(x12~x16, data=data, na.action=na.omit)$fitted.value
  data$x12[na_ind]<-fit[na_ind]
  na_ind<-which(is.na(data$x12))
  data$x12[na_ind]<-mean(na.omit(data$x12))
  
  
  #차입금
  s4=(data$x13/100)*s3
  #고정자산
  s5=(data$x12/100)*s4
  
  ## x3 고정비율 = 고정자산/ 자기자본
  na_ind<-which(is.na(data$x3))
  data$x3[na_ind]<-(s5[na_ind]/(s3[na_ind]))*100
  na_ind<-which(is.na(data$x3))
  data$x3[na_ind]<-(s5[na_ind]/((s3[na_ind]+exp(-29))))*100
  
  ## x41 로그 매출액
  na_ind <- which(is.na(data$x41))
  fit<-lm(data$x41~data$x42)$fitted.value
  data$x41[na_ind]<-fit[na_ind]
  
  
  ## x14 고정재무비보상비율=(법인세비용차감전순이익+이자비용+고정재무비용)/(이자비용+고정재무비용)
  na_ind<-which(is.na(data$x14))
  data$x14[na_ind]<-mean(na.omit(data$x14))
  
  ## x35 매출채권회전율 365/(매출액/매출채권)
  na_ind <- which(is.na(data$x35))
  data$x35[na_ind]<-mean(na.omit(data$x35))
  
  ## x15 총차입금/(총차입금+자기자본)비율
  na_ind <- which(is.na(data$x15))
  data$x15[na_ind]<-(s4[na_ind]/(s4[na_ind]+s3[na_ind]))*100
  
  #고정부채비율 = (고정부채)/(자기자본)*100
  #x2
  idx2.2 <- which(is.na(data$x2))
  lm2 <- lm(data$x2~data$x3,na.action=na.omit)
  data$x2[idx2.2]<-predict(lm2, newdata=data.num)[idx2.2]
  
  ## x43 cate 숫자로 변환
  which_1=which(data$x43 == "경공업")
  which_2=which(data$x43 == "중공업")
  which_3=which(data$x43 == "건설업")
  which_4=which(data$x43 == "도소매")
  which_5=which(data$x43 == "서비스")
  data$x43[which_1]<-1
  data$x43[which_2]<-2
  data$x43[which_3]<-3
  data$x43[which_4]<-4
  data$x43[which_5]<-5
  data$x43<-as.numeric(data$x43)
  ## x44 cate 숫자로변환
  which_1=which(data$x44 == "개인")
  which_2=which(data$x44 == "소호")
  which_3=which(data$x44 == "비외감2")
  which_4=which(data$x44 == "비외감1")
  which_5=which(data$x44 == "외감")
  data$x44[which_1]<-1
  data$x44[which_2]<-2
  data$x44[which_3]<-3
  data$x44[which_4]<-4
  data$x44[which_5]<-5
  data$x44<-as.numeric(data$x44)
  
  
  
  data$x45 <- s1*data$x5/100 # 총부채 = 총자산*(부차총계/자산총계비율)
  data$x46 <- s1-s2 # 총 자기자본 = 총자산 - 총부채
  data$x47 <- s3*data$x26/100 # 순이익 = 총자기자본*자기자본순이익율/100
  data$x48 <- s3*data$x27*100 # 자본금 = 총자기자본*자본금순이익율/100
  data$x49 <- s2*data$x25/100 # 금융비용 = 총부채*(금융비용/총부채비율)/100
  
  
  return(data)
  
}

pre_data<-preprocessing(data)

#train, test split
train<- pre_data[1:3168,]
train <- train[ ,!(colnames(train) == c('x34','x35'))]
test<- pre_data[3169:nrow(pre_data),]
test <- test[ ,!(colnames(test) == c('x34','x35'))]


### glm, gam 전용 데이터
cate<-c("x43","y")
cate2<-c("x43")

pre_data2=pre_data%>%mutate_at(vars(-cate),as.numeric)
pre_data2=pre_data%>%mutate_at(vars(-cate),center)
pre_data2[cate2]=lapply(pre_data[cate2],factor)

train.glm<-pre_data2[1:3168,c(46,2:34,37:45, 48:52)]
test.glm<-pre_data2[3169:nrow(pre_data),c(46,2:34,37:45, 48:52)]

### cox 전용 데이터
cate<-c("x43","y","calendarTime","time")
cate2<-c("x43")
pre_data$time<-pre_data$x40+pre_data$calendarTime/365
na_ind<-which(is.na(pre_data$time))
pre_data$time[na_ind]<-mean(pre_data$time, na.rm=TRUE)

pre_data=pre_data%>%mutate_at(vars(-cate),as.numeric)
pre_data=pre_data%>%mutate_at(vars(-cate),center)
pre_data[cate2]=lapply(pre_data[cate2],factor)

train.cox<-pre_data[1:3168,c(46,2:34,37:45, 48:53)]
test.cox<-pre_data[3169:nrow(data),c(46,2:34,37:45, 48:53)]


##```````````````````````모델 학습```````````````````````````````````````
## gam
glm_probit<-glm(formula = y ~ x1 + x5 + x6 + x7 + x10 + x13 + x18 + x20 + 
                  x22 + x24 + x25 + x26 + x28 + x32 + x33 + x39 + x40 + x44 + 
                  x46 + x48, family = binomial(link = "probit"), data = train.glm)


## glm
gam_probit<-gam(formula = y ~ s(x49) + x5 + x6 + x10 + x18 + x20 + x22 +
                  x24 + x25 + x26 + x28 + x32 + x33 +x39 + x41 + x44 + x46,
                family = binomial(link = "probit"), data = train.glm,
                na.action = na.omit,  select = TRUE, trace = FALSE)


## cox
cox.model<-coxph(formula = Surv(time, y) ~ x1 + x3 + x6 + x8 + x10 + x11 + x14 + x18 + x24 + 
                   x25 + x29 + x33 + x39 + x44 + x46 + x48 + x49 + x2,data = (train.cox))



## svm
#svm.res <- svm(y=train$y, x=train[, !names(train) %in% c("y","calendarTime")], 
               #type = "C-classification", probability=TRUE)

## random forest
#rf.res <- randomForest(y=as.factor(train$y), x=train[, !names(train) %in% c("y","calendarTime")], 
#mtry = floor(sqrt(ncol(train))), ntree=111, importance = T, keep.forest=TRUE)

## nn
nn.model <- nnet(y=train$y, x=train[, !names(train) %in% c("y","calendarTime")], size=5, decay=0.1)


## knn
knn_predict <- knn(train=train[,-c(1,44,45)], test=test[,-c(1,44,45)], cl=train$y, k=59, prob=T)
#knn_prob <-1-attr(knn_predict,"prob") #확률

## xgb
xgb_model <- xgboost(data=data.matrix(train[,-c(1,44,45)]), label=train$y, nrounds = 10)
#xgb_predict <- predict(xgb_model, data.matrix(test[,-c(1,44,45)])) #확률
#xgb_predict[1:10]

preprocessing2<- function(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,
                          x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,
                          x23,x24,x25,x26,x27,x28,x29,x30,x31,x32,x33,
                          x34,x35,x36,x37,x38,x39,x40,x41,x42,x43,x44){
  data<-data.frame(matrix(c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,
                            x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,
                            x23,x24,x25,x26,x27,x28,x29,x30,x31,x32,
                            x33,x34,x35,x36,x37,x38,x39,x40,x41,x42,x43,x44),,44))
  
  colnames(data)<-c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12",
                    "x13","x14","x15","x16","x17","x18","x19","x20","x21","x22",
                    "x23","x24","x25","x26","x27","x28","x29","x30","x31","x32",
                    "x33","x34","x35","x36","x37","x38","x39","x40","x41","x42","x43","x44")
  cate<-c("x43")
  
  data=data%>%mutate_at(vars(-cate),as.numeric)
  
  s1 <- exp(data$x42) # 총자산 = exp(로그자산)
  s2 <- s1*data$x5/100 # 총부채 = 총자산*(부차총계/자산총계비율)
  s3 <- s1-s2 # 총 자기자본 = 총자산 - 총부채
  s4 <- s3*data$x26/100 # 순이익 = 총자기자본*자기자본순이익율/100
  s5 <- s3*data$x27*100 # 자본금 = 총자기자본*자본금순이익율/100
  s6 <- s2*data$x25/100 # 금융비용 = 총부채*(금융비용/총부채비율)/100
  
  data$x45 <- s1*data$x5/100 # 총부채 = 총자산*(부차총계/자산총계비율)
  data$x46 <- s1-s2 # 총 자기자본 = 총자산 - 총부채
  data$x47 <- s3*data$x26/100 # 순이익 = 총자기자본*자기자본순이익율/100
  data$x48 <- s3*data$x27*100 # 자본금 = 총자기자본*자본금순이익율/100
  data$x49 <- s2*data$x25/100 # 금융비용 = 총부채*(금융비용/총부채비율)/100
  
  
  return(data)
  
}


##````````````````````````````````````````````````````````
header <- dashboardHeader(title='기업부도 예측')
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('기업부도 예측', tabName = 'part5', icon = icon('won')),
    radioButtons('model','MODEL', selected = '1', choices = c('GAM'='1','GLM'='2','COX'='3','NN' = '5','KNN'='6','XGB'='7')),
    valueBoxOutput('p',width = 180),
    id = 'tabs'
    
  )
)

## Body
body <- dashboardBody(
  
  tabItems(
    tabItem('part5',
            tabBox(title='Data', width = 12, height = 500,
                   tabPanel(title='1', status = 'primary', width = 4, height = 400, solidHeader = F,
                            box(status = 'primary', width = 3, height = 300, solidHeader = F,
                                sliderInput('x37', '자본금회전율', value = 2, min = 0, max = 50000, step = 0.01),
                                sliderInput('x38', '재고자산회전율', value = 20, min = 0, max = 90000, step = 0.01),
                                sliderInput('x39', '총자본회전율', value = 0.5, min = 0, max = 43, step = 0.01)),
                            
                            box(status = 'primary', width =3, height = 300, solidHeader = F,
                                sliderInput('x40', '기업나이', value = 3600, min = 450, max = 40000, step = 1),
                                sliderInput('x41', '로그매출액', value = 14, min = 2, max = 25, step = 0.01),
                                sliderInput('x42', '로그자산', value = 5, min = 0, max = 25, step = 0.01)),
                            
                            
                            box(title = '업종', status = 'primary', width =3, height = 300, solidHeader = F,
                                radioButtons('x43','', selected = 1, choices = c('경공업'=1,'중공업'=2,'건설업'=3,'도소매'=4,'서비스'=5))),
                            box(title = '규모', status = 'primary', width =3, height = 300, solidHeader = F,
                                radioButtons('x44','', selected = 2, choices = c('외감'=1,'비외감1'=2,'비외감2'=3,'소호'=4,'개인'=5)))
                   ),        
                   tabPanel("2",
                            box( status = 'primary', width = 4, height = 300, solidHeader = F,
                                 sliderInput('x1', '금융비용/총비용비율', value = 5, min = 0, max = 100,step = 0.01),
                                 sliderInput('x2', '고정부채비율', value = 146, min = -20, max = 2000000,step = 0.01),
                                 sliderInput('x3','고정비율', value = 180, min = 0, max = 2000000, step =0.01),
                                 sliderInput('x4','부채비율', value = 140, min = 0, max = 50000, step =0.01)),
                            box( status = 'primary', width = 4, height = 300, solidHeader = F,                     
                                 sliderInput('x5', '부채총계/자산총계비율', value = 60, min = 0, max = 1000,step = 0.01),
                                 sliderInput('x6', '순부채/총자산비율', value = 50, min = -100, max = 1000,step = 0.01),
                                 sliderInput('x7','유동부채비율', value = 0.16, min = 0, max = 400000, step =0.01),
                                 sliderInput('x8','유동비율', value = 40, min =35000, max = 4000000, step =0.01)),
                            box( status = 'primary', width = 4, height = 300, solidHeader = F,     
                                 sliderInput('x9', '유보액/총자산비율', value = 0.6, min = -2000, max = 500,step = 0.01),
                                 sliderInput('x10', '자기자본비율', value = 40, min = -1000, max = 1000,step = 0.01),
                                 sliderInput('x11','차입금의존도', value = 60, min = 0, max = 1000, step =0.01),
                                 sliderInput('x12','고정자산/차입금비율', value = 120, min = -500, max = 1000000, step =0.01))),
                   
                   tabPanel("3",         
                            box( status = 'primary', width = 4, height = 300, solidHeader = F,
                                 sliderInput('x13', '차입금/자기자본비율', value =120, min = 0, max = 500000,step = 0.01),
                                 sliderInput('x14', '고정재무비보상배율', value = 1, min = -1000, max = 1000,step = 0.01),
                                 sliderInput('x15','총차입금/(총차입금+자기자본)비율', value =60, min = 0, max = 100000, step =0.01),
                                 sliderInput('x16','총CF/차입금비율', value = 5, min = -500000, max = 500000, step =0.01)),
                            box( status = 'primary', width = 4, height = 300, solidHeader = F,                     
                                 sliderInput('x17', 'CF/차입금비율', value = -20, min = -100000, max = 1000000,step = 0.01),
                                 sliderInput('x18', '순운전자본/총자산비율', value = 25, min = -1000, max = 1000,step = 0.01),
                                 sliderInput('x19','유동부채구성비율', value = 0.5 , min = 0, max = 1000, step =0.01),
                                 sliderInput('x20','현금비율', value = 100, min = 0, max = 500000, step =0.01)),
                            box( status = 'primary', width = 4, height = 300, solidHeader = F,                       
                                 sliderInput('x21', '총자산투자효율', value = 20, min = -100, max = 100000,step = 0.01),
                                 sliderInput('x22', '매출채권증가율', value = 2, min = -1000, max = 10^10,step = 0.01),
                                 sliderInput('x23','재고자산증가율', value = -30, min = -100, max = 10^12, step =0.01),
                                 sliderInput('x24','경영자본순이익율', value =2, min = -500, max = 1000, step =0.01))),
                   tabPanel("4",   
                            box( status = 'primary', width = 4, height = 300, solidHeader = F,
                                 sliderInput('x25', '금융비용/총부채비율', value = 4, min = 0, max = 1000,step = 0.01),
                                 sliderInput('x26', '자기자본순이익', value = 4, min = -100000, max = 100000,step = 0.01),
                                 sliderInput('x27','자본금순이익율', value = 4, min = -10000, max = 1000000, step =0.01),
                                 sliderInput('x28','총자본경상이익율', value = 2, min = -200, max = 1000, step =0.01)),
                            box( status = 'primary', width = 4, height = 300, solidHeader = F,
                                 sliderInput('x29', '총자본순이익전율', value = 2, min = -150, max = 600, step = 0.01),
                                 sliderInput('x30', '총자본영업이익율', value = 3, min = -260, max = 600, step = 0.01),
                                 sliderInput('x31', '총자산사업이익율', value = 4, min = -200, max = 600, step = 0.01),
                                 sliderInput('x32', '경영자본회전율', value = 0.5, min = 0, max = 63, step = 1)),
                            box( status = 'primary', width = 4, height = 300, solidHeader = F,
                                 sliderInput('x33', '고정자산회전율', value =0.6 , min = 0, max = 10000, step = 0.01),
                                 sliderInput('x34', '매입채무회전율', value = 10000, min = 0, max = 1000000, step = 0.01),
                                 sliderInput('x35', '매출채권회전율', value = 50, min = 0, max = 100000, step = 0.01),
                                 sliderInput('x36', '자기자본회전율', value = 2, min = 0, max = 2000, step = 1)))
                   
                   
            ))))

ui <- dashboardPage(header, sidebar, body)



server <- function(input, output, session) {
  
  #일반 데이터
  d <- reactive({
    input.data<- preprocessing2(input$x1, input$x2, input$x3, input$x4, input$x5, input$x6, input$x7, input$x8, input$x9, input$x10, input$x11, input$x12, input$x13, input$x14, input$x15, input$x16, input$x17, input$x8, input$x19, input$x20, input$x21, input$x22, input$x23, input$x24, input$x25, input$x26, input$x27, input$x28, input$x29, input$x30, input$x31, input$x32, input$x33, input$x34, input$x35, input$x36, input$x37, input$x38, input$x39, input$x40, input$x41, input$x42, input$x43, input$x44)
    
    return(input.data)
  })
  
  #glm, gam 전용 데이터
  d.glm <- reactive({
    cleaned<- preprocessing2(input$x1, input$x2, input$x3, input$x4, input$x5, input$x6, input$x7, input$x8, input$x9, input$x10, input$x11, input$x12, input$x13, input$x14, input$x15, input$x16, input$x17, input$x8, input$x19, input$x20, input$x21, input$x22, input$x23, input$x24, input$x25, input$x26, input$x27, input$x28, input$x29, input$x30, input$x31, input$x32, input$x33, input$x34, input$x35, input$x36, input$x37, input$x38, input$x39, input$x40, input$x41, input$x42, input$x43, input$x44)
    
    cate<-c("x43")
    
    cleaned=cleaned%>%mutate_at(vars(-cate),as.numeric)
    cleaned=cleaned%>%mutate_at(vars(-cate),center)
    cleaned[cate2]=lapply(cleaned[cate2],factor)
    
    
    return(cleaned)
  })
  
  #cox 전용 데이터
  d.cox <- reactive({
    cleaned<- preprocessing2(input$x1, input$x2, input$x3, input$x4, input$x5, input$x6, input$x7, input$x8, input$x9, input$x10, input$x11, input$x12, input$x13, input$x14, input$x15, input$x16, input$x17, input$x8, input$x19, input$x20, input$x21, input$x22, input$x23, input$x24, input$x25, input$x26, input$x27, input$x28, input$x29, input$x30, input$x31, input$x32, input$x33, input$x34, input$x35, input$x36, input$x37, input$x38, input$x39, input$x40, input$x41, input$x42, input$x43, input$x44)
    
    cate2<-c("x43")
    
    cleaned$time<-cleaned$x40+0.5
    cleaned$y<-1
    cleaned$calendarTime<-0
    
    cleaned=cleaned%>%mutate_at(vars(-cate),as.numeric)
    cleaned=cleaned%>%mutate_at(vars(-cate),center)
    cleaned[cate2]=lapply(cleaned[cate2],factor)
    
    
    return(cleaned)
  })
  
  
  
  ## GLM
  glm <- reactive({
    data <- d.glm()
    glm_p<-predict(glm_probit, data, type="response")   
    return(glm_p)
  })
  
  ## GAM
  gam <- reactive({
    data <- d.glm()
    gam_p<-predict(gam_probit, data, type="response")    
    return(gam_p)
  })
  
  ## COX                                              
  cox <- reactive({
    data <- d.cox()
    cox_p<-1-predict(cox.model, data, type="survival")
    return(cox_p)
  })
  
  ## SVM
  #svm <- reactive({
    #data <- d()
    #svm.pred <- attr(predict(svm.res, x=data, 
                             #decision.values = TRUE, probability = TRUE), "probabilities")[, 2]
    #return(svm.pred)
  #})
  
  ## NN
  nn <- reactive({
    data <- d()
    nn.pred <- predict(nn.model, x=data, type="raw")[1]
    return(nn.pred)    
  })
  
  
  ## KNN
  knn <- reactive({
    data <- d()
    knn_prob <-1-attr(knn_predict,"prob")[1]
    return(knn_prob)
  })
  
  ## XGB
  xgb <- reactive({
    data <- d()
    xgb_predict <- predict(xgb_model, data.matrix(data[, !names(data) %in% c('x34',"x35")]))
    #xgb_predict <- predict(xgb_model, data.matrix(test[,-c(44,45)])) #확률
    
    return(xgb_predict)
  })
  
  ## LDA
  lda <- reactive({
    data <- d()
    predict_lda <- predict(lda_train_fit, newdata=data[, !names(data) %in% c('ID',"y","calendarTime")])
    predict_lda <- 1-predict_lda$posterior[,1]
    return(predict_lda)
  })
  
  
  
  # 선택된 모델 결과
  result <- reactive({
    if (input$model == '1'){
      return(gam())
    } else if (input$model == '2'){
      return(glm())
    } else if (input$model == '3'){
      return(cox())
    } else if (input$model == '4'){
      return(svm())
    } else if (input$model == '5'){
      return(nn())
    } else if (input$model == '6'){
      return(knn())
    } else if (input$model == '7'){
      return(xgb())
    } 
  })
  
  
  output$p <- renderValueBox({
    valueBox(round(result(),6), "Probability", color = "aqua", width=6)
  })
  
  
}

shinyApp(ui, server)