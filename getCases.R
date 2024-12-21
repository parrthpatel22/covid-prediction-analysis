#Loading required Library
library(rgdal)
library(ggplot2)
library(tidyverse)
library(gpclib)
library(readr)
library(broom)
library(minpack.lm)
set.seed(50)

#Function which performs data pre-reading, data cleaning and returns model-ready dataframe.
getConfirmed <- function(date)
{
  d1 = as.Date("31/01/2020", format="%d/%m/%Y")
  d2 = as.Date(date, format="%d/%m/%Y")
  n = as.double(difftime(d2,d1,units = "days"))
  print(n)
  ds <- read_csv("C:/Users/College/Documents/CovidDataset/covid_19_2021.csv")
  ds_fact <- levels(ordered(ds$Date))
  total_c <- data.frame(date = as.character(ds_fact))
  total_c$cases <- 0
  for( i in 1:length(ds_fact))
  {
    column <- ds[ds$Date == ds_fact[i],]
    total_c[total_c$date == ds_fact[i],]$cases <- sum(column$Confirmed)
  }
  total_c
  total_c <- total_c[order(as.Date(total_c$date, format="%d/%m/%Y")),]
  total_c <- total_c[order(total_c$cases),]
  total_c$days <- 0
  for(i in 1:length(total_c$cases)){
    total_c[i,]$days <- i
  }
  total_c$days
  total_c <- total_c[,-1]
  
  plot(total_c$days[1:n],total_c$cases[1:n],main="Cases VS Days",xlab="Days",ylab ="Cases",type="l")
  
  d <- data.frame(y = total_c$cases[1:n],t=total_c$days[1:n])
  return (d)
}

ds <- getConfirmed("15/02/2020")
getActive <- function(date)
{
  ds <- read_csv("C:/Users/College/Downloads/covid_19_india (1).csv")
  ds_fact <- levels(ordered(ds$Date))
  total_c <- data.frame(date = as.character(ds_fact))
  total_c$cases <- 0
  for( i in 1:length(ds_fact))
  {
    column <- ds[ds$Date == ds_fact[i],]
    total_c[total_c$date == ds_fact[i],]$cases <- sum(column$Confirmed) - sum(column$Cured)
  }
  total_c
  total_c <- total_c[order(as.Date(total_c$date, format="%d/%m/%Y")),]
  total_c <- total_c[order(total_c$cases),]
  total_c$days <- 0
  for(i in 1:length(total_c$cases)){
    total_c[i,]$days <- i
  }
  total_c$days
  total_c <- total_c[,-1]
  
  plot(total_c$days,total_c$cases,main="Cases VS Days",xlab="Days",ylab ="Cases",type="l")
  d <- data.frame(y = total_c$cases,t=total_c$days)
  return (d)
}
set.seed(50)
#Function which uses nlsLM() to predict Cases using Gompertz Model
gompertz <-function(date)
{
  d <-getConfirmed(date)
  starting.values <- c(a = 10000, 
                       b=1, 
                       c=1)
  formula <- "y~a*exp(-b*exp(-c*t))"
  result <- nlsLM(formula, data = d, starting.values,control = list(maxiter = 500))
  print(predict(result,data.frame(t=length(d$y)+1)))
  t1 <- 1:600
  t1 <- data.frame(t = t1)
  Y<-predict(result,t1)
  print(result)
  getR2(d$y,Y,length(d$y))
  plot(t1$t,Y,main="Cases vs Days",type="l",col="red",xlab="Days",ylab="Cases")
  lines(d$t,d$y,col = "blue")
  legend("topleft",legend=c("Real Curve","Gompertz"),col=c("blue","red"),lty=c(1,2),bg="transparent")
  
}

#Function which uses nlsLM() to predict Cases using Logistic Model
logistic <-function(date)
{
  d <-getConfirmed(date)
  starting.values <- c(a = 10000, 
                       b=1, 
                       c=1)
  formula <- "y~(a/(1+exp(b - (c*t))))"
  result <- nlsLM(formula, data = d, starting.values,control = list(maxiter = 500))
  print(predict(result,data.frame(t=length(d$y)+1)))
  t1 <- 1:600
  t1 <- data.frame(t = t1)
  Y<-predict(result,t1)
  print(result)
  print("Yes")
  print(paste("n: ",length(d$y)))
  getR2(d$y,Y,length(d$y))
  
  plot(t1$t,Y,main="Cases vs Days",type="l",col="red",xlab="Days",ylab="Cases")
  lines(d$t,d$y,col = "blue")
  legend("topleft",legend=c("Real Curve","Logistic"),col=c("blue","red"),lty=c(1,2),bg="transparent")
  
}


#Function which uses nlsLM() to predict Cases using Bertalanffy Model
bertalanffy <-function(date)
{
  d <-getConfirmed(date)
  starting.values <- c(a = 10000, 
                       b=1, 
                       c=1)
  formula <- "y~a*((1-(exp(-b*t)))^c)"
  result <- nlsLM(formula, data = d, starting.values,control = list(maxiter = 500))
  print(predict(result,data.frame(t=length(d$y)+1)))
  t1 <- 1:600
  t1 <- data.frame(t = t1)
  Y<-predict(result,t1)
  print(result)
  getR2(d$y,Y,length(d$y))
  plot(t1$t,Y,main="Cases vs Days",type="l",col="red",xlab="Days",ylab="Cases")
  lines(d$t,d$y,col = "blue")
  legend("topleft",legend=c("Real Curve","Bertalanffy"),col=c("blue","red"),lty=c(1,2),bg="transparent")
}

#Function for generating R2
getR2 <- function(d,p,n)
{
  m <- sum(d)/n
  num <- 0
  den <- 0
  for(i in 1:n)
  {
    num <- num + (((p[i] - d[i])*(p[i] - d[i])))
    den <- den + ((d[i] - m)*(d[i] - m))
  }
  r2 <- num/den
  r2 <- 1 - r2
  print(paste("R2 : ",r2))
}


#7 Days After
gompertz("20/05/2020")
logistic("20/05/2020")
bertalanffy("11/11/2020")