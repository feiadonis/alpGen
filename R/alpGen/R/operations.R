library(icsUtil)
library(plyr)
library(abind)
library(TTR)
library(zoo)

Add <- function(x,y){
  return(x+y)
}

Minus <- function(x,y){
  return(x-y)
}

Multiply <- function(x,y){
  return(x*y)
}

Divide <- function(x,y){
  return(x/y)
}

Sqrt <- function(x){
  return(sqrt(x))
}

Power <- function(x,N){
  return(x^N)
}

Abs <- function(x){
  return(abs(x))
}

Log <- function(x){
  return(Log(x))
}

Sign <- function(x){
  return(sign(x))
}

Rank <- function(x){
  data = aaply(x,c(2,3,4),rank,na.last="keep",.drop=FALSE)
  data = aperm(data,c(4,1,2,3))
  return(data)
}

Ts_rank <- function(x,N){

}
