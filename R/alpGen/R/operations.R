library(icsUtil)
library(plyr)
library(abind)
library(TTR)
library(zoo)

add <- function(x,y){
  if(is.array(x) || is.numeric(x)) data1 = x
  else data1 = data[,,,x,drop = FALSE]
  if(is.array(y) || is.numeric(y)) data2 = y
  else data2 = data[,,,y,drop = FALSE]
  data = data1 + data2
  return(data)
}

minus <- function(x,y){
  if(is.array(x) || is.numeric(x)) data1 = x
  else data1 = data[,,,x,drop = FALSE]
  if(is.array(y) || is.numeric(y)) data2 = y
  else data2 = data[,,,y,drop = FALSE]
  data = data1 - data2
  return(data)
}

multiply <- function(x,y){
  if(is.array(x) || is.numeric(x)) data1 = x
  else data1 = data[,,,x,drop = FALSE]
  if(is.array(y) || is.numeric(y)) data2 = y
  else data2 = data[,,,y,drop = FALSE]
  data = data1*data2
  return(data)
}

divide <- function(x,y){
  if(is.array(x) || is.numeric(x)) data1 = x
  else data1 = data[,,,x,drop = FALSE]
  if(is.array(y) || is.numeric(y)) data2 = y
  else data2 = data[,,,y,drop = FALSE]
  data = data1 / data2
  return(data)
}

mysqrt <- function(x){
  if(is.array(x) || is.numeric(x)) data1 = x
  else data1 = data[,,,x,drop=FALSE]
  data = sqrt(data)
  return(data)
}

mypower <- function(x,N){
  if(!is.numeric(N))stop("input N must be a numberic!")
  if(is.array(x) || is.numeric(x)) data1 = x
  else data1 = data[,,,x,drop=FALSE]
  data = data1^N
  return(data)
}

myabs <- function(x){
  if(is.array(x) || is.numeric(x)) data1 = x
  else data1 = data[,,,x,drop=FALSE]
  data = abs(data1)
  return(data)
}

mylog <- function(x){
  if(is.array(x) || is.numeric(x)) data1 = x
  else data1 = data[,,,x,drop=FALSE]
  data = log(data1)
  return(data)
}

mysign <- function(x){
  if(is.array(x) || is.numeric(x)) data1 = x
  else data1 = data[,,,x,drop=FALSE]
  data = sign(data1)
  return(data)
}

