Add <- function(x,y){
  data1 = vreplace(x)
  data2 = vreplace(y)
  data = data1 + data2
  return(data)
}

Minus <- function(x,y){
  data1 = vreplace(x)
  data2 = vreplace(y)
  data = data1 - data2
  return(data)
}

Product <- function(x,y){
  data1 = vreplace(x)
  data2 = vreplace(y)
  data = data1*data2
  return(data)
}

Divide <- function(x,y){
  data1 = vreplace(x)
  data2 = vreplace(y)
  data = data1 / data2
  return(data)
}

Sqrt <- function(x){
  data1 = vreplace(x)
  data = sqrt(data)
  return(data)
}

Abs <- function(x){
  data1 = vreplace(x)
  data = abs(data1)
  return(data)
}

Rank.naLast <- function(x){
  data1 = vreplace(x)
  data1 <- aaply(data1,c(2,3,4),rank,na.last=TRUE,.drop=FALSE)
  data1 <- aperm(data1,c(4,1,2,3))
  names(dimnames(data1)) = c("K","D","T","V")
  return(data1)
}

Rank.naKeep <- function(x){
  data1 = vreplace(x)
  data1 <- aaply(data1,c(2,3,4),rank,na.last = "keep",.drop=FALSE)
  data1 <- aperm(data1,c(4,1,2,3))
  names(dimnames(data1)) = c("K","D","T","V")
  return(data1)
}

Ts_delay <- function(x,N){
  data1 = vreplace(x)
  data1 <- aaply(data1,c(1,3,4),shift,n=N,.drop=FALSE)
  data1 <- aperm(data1,c(1,4,2,3))
  names(dimnames(data1)) = c("K","D","T","V")
  dimnames(data1)$D <- dimnames(data)$D
  return(data1)
}

Ts_diff <- function(x,N){
  data1 = vreplace(x)
  data1 <- aaply(data1,c(1,3,4),diff,lag=N,.drop=FALSE)
  data1 <- aperm(data1,c(1,4,2,3))
  names(dimnames(data1)) = c("K","D","T","V")
  dimnames(data1)$D <- dimnames(data)$D[-(1:N)]
  dataPad <- array(NA, c(dim(data1)[1],N,dim(data1)[3],dim(data1)[4]),
                   dimnames = list(K = dimnames(data1)$K,
                                   D = dimnames(data1)$D[1:N],
                                   T = dimnames(data1)$T,
                                   V = dimnames(data1)$V))
  data1 <- panel.combine(list(dataPad,data1))
  return(data1)
}

Scale <- function(x){
  data1 = vreplace(x)
  data1[is.infinite(data1)] = NA
  data1 <- aaply(data1,c(2,3,4),function(x)(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE),.drop=FALSE)
  data1 <- aperm(data1,c(4,1,2,3))
  names(dimnames(data1)) = c("K","D","T","V")
  return(data1)
}

Square <- function(x){
  data1 = vreplace(x)
  data1 <- data1^2
  return(data1)
}

Cube <- function(x){
  data1 = vreplace(x)
  data1 <- data1^3
  return(data1)
}

Lt <- function(x,y){
  data1 = vreplace(x)
  data2 = vreplace(y)
  return(data1 < data2)
}

Gt <- function(x,y){
  data1 = vreplace(x)
  data2 = vreplace(y)
  return(data1 > data2)
}

Eq <- function(x,y){
  data1 = vreplace(x)
  data2 = vreplace(y)
  return(data1 == data2)
}

Lte <- function(x,y){
  data1 = vreplace(x)
  data2 = vreplace(y)
  return(data1 <= data2)
}

Gte <- function(x,y){
  data1 = vreplace(x)
  data2 = vreplace(y)
  return(data1 >= data2)
}

Negative <- function(x){
  data1 = vreplace(x)
  return(-data1)
}

And <- function(x,y){
  data1 = vreplace(x)
  data2 = vreplace(y)
  return(data1 & data2)
}

Or <- function(x,y){
  data1 = vreplace(x)
  data2 = vreplace(y)
  return(data1 | data2)
}

Not <- function(x){
  data1 = vreplace(x)
  return(!data1)
}

Same <- function(x){
  data1 = vreplace(x)
  return(data1)
}

Log <- function(x){
  data1 = vreplace(x)
  data = log(data1)
  return(data)
}

Sign <- function(x){
  data1 = vreplace(x)
  data = sign(data1)
  return(data)
}

Exp <- function(x){
  data1 = vreplace(x)
  return(exp(data1))
}

Ts_max <- function(x,N){
  data1 = vreplace(x)
  datanew <- aaply(data1,c(1,3,4),function(x){
    if(sum(!is.na(x)) >= N){
      c(RollingMax(x,window = N, na_method = "ignore"))
    }else{
      rep(NA,length(x))
    }
  },.drop=FALSE)
  datanew <- aperm(datanew,c(1,4,2,3))
  names(dimnames(datanew)) = c("K","D","T","V")
  dimnames(datanew)$D <- dimnames(data1)$D
  return(datanew)
}

Ts_min <- function(x,N){
  data1 = vreplace(x)
  datanew <- aaply(data1,c(1,3,4),function(x){
    if(sum(!is.na(x)) >= N){
      c(RollingMin(x,window = N, na_method = "ignore"))
    }else{
      rep(NA,length(x))
    }
  },.drop=FALSE)
  datanew <- aperm(datanew,c(1,4,2,3))
  names(dimnames(datanew)) = c("K","D","T","V")
  dimnames(datanew)$D <- dimnames(data1)$D
  return(datanew)
}

Ts_corr <- function(x,y,N){
  data1 = vreplace(x)
  data2 = vreplace(y)
  dimnames(data1)[['V']] = "X"
  dimnames(data2)[['V']] = "Y"
  datanew = panel.combine(list(data1,data2))

  result <- aaply(datanew, c(1,3), function(x){
    x1 = x[,1]
    x1[is.na(x1)] = 0
    x2 = x[,2]
    x2[is.na(x2)] = 0
    y = c(RollingCorr(x1,x2,window = N))
    data = array(0,dim = c(length(y),1),dimnames = list(dimnames(datanew)[[2]],"X"))
    data[,1] = y
    return(data)
  }, .drop=FALSE)
  result = aperm(result,c(1,3,2,4))
  names(dimnames(result)) = c("K","D","T","V")
  return(result)
}

Ts_rank <- function(x,N){
  data1 = vreplace(x)
  datanew <- aaply(data1,c(1,3,4),function(x){
    ifNA <- is.na(x)
    nonNA.x <- x[!ifNA]
    NAidx <- which(ifNA)
    x[!ifNA] <- ts_rank(nonNA.x,N=N)
    x
  },.drop=FALSE)
  result = aperm(datanew,c(1,4,2,3))
  names(dimnames(result)) = c("K","D","T","V")
  dimnames(result)$D <- dimnames(data1)$D
  return(result)
}

roll_argmax <- rollit_raw("
                          int out=0;
                          double max = 0;
                          for(int i =  0;i < n; i++){
                          if(i==0){out=1;}
                          if(X(i) > max){ max = X(i); out = i+1;}
                          }
                          return out;
                          ")

roll_argmaxr <- function(x,n=1L,fill=NA){
  return(c(rep(fill,n-1),roll_argmax(x,n)))
}

Ts_argmax <- function(x,N){
  data1 = vreplace(x)
  datanew = aaply(data1,c(1,3,4),roll_argmax,N,.drop=FALSE)
  result = aperm(datanew,c(1,4,2,3))
  names(dimnames(result)) = c("K","D","T","V")
  dimnames(result)$D = dimnames(data1)$D
  return(result)
}

Ts_argmin <- function(x,N){
  data1 = vreplace(x)
  dtanew = -aaply(-data1,c(1,3,4),roll_argmaxr,N,.drop=FALSE)
  result = aperm(datanew,c(1,4,2,3))
  names(dimnames(result)) = c("K","D","T","V")
  dimnames(result)$D = dimnames(data1)$D
  return(result)
}

Ts_mean <- function(x,N){
  data1 = vreplace(x)
  datanew = aaply(data1,c(1,3,4),function(x){
    if(sum(!is.na(x)) >=N){
      c(RollingMean(x,window = N, na_method = "ignore"))
    }else{
      rep(NA,length(x))
    }
  },.drop=FALSE)
  result = aperm(datanew,c(1,4,2,3))
  names(dimnames(result)) = c("K","D","T","V")
  dimnames(result)$D = dimnames(data1)$D
  return(result)
}

Ts_std <- function(x,N){
  data1 = vreplace(x)
  datanew = aaply(data1,c(1,3,4),function(x){
    if(sum(!is.na(x))>=N){
      c(RollingStd(x,window = N, na_method = "ignore"))
    }else{
      rep(NA,length(x))
    }
  },.drop=FALSE)
  result = aperm(datanew,c(1,4,2,3))
  names(dimnames(result)) = c("K","D","T","V")
  dimnames(result)$D = dimnames(data1)$D
  return(result)
}

Ts_sum <- function(x,N){
  data1 = vreplace(x)
  datanew = aaply(data1, c(1,3,4), function(x){
    if(sum(!is.na(x))>=N){
      c(RollingSum(x,window=N,na_method="ignore"))
    }else{
      rep(NA,length(x))
    }
  },.drop=FALSE)
  result = aperm(datanew,c(1,4,2,3))
  names(dimnames(result)) = c("K","D","T","V")
  dimnames(result)$D = dimnames(data1)$D
  return(result)
}

Ts_var <- function(x,N){
  data1 = vreplace(x)
  datanew = aaply(data1,c(1,3,4),function(x){
    if(sum(!is.na(x))>=N){
      c(RollingVar(x,window=N,na_method="ignore"))
    }else{
      rep(NA,length(x))
    }
  },.drop=FALSE)
  result = aperm(datanew,c(1,4,2,3))
  names(dimnames(result)) = c("K","D","T","V")
  dimnames(result)$D <- dimnames(data1)$D
  return(result)
}

Ts_median <- function(x,N){
  data1 = vreplace(x)
  datanew = aaply(data1,c(1,3,4),function(x){
    if(sum(!is.na(x))>=N){
      c(RollingMedian(x,window=N,na_method="ignore"))
    }else{
      rep(NA,length(x))
    }
  },.drop=FALSE)
  result = aperm(datanew,c(1,4,2,3))
  names(dimnames(result)) = c("K","D","T","V")
  dimnames(result)$D <- dimnames(data1)$D
  return(result)
}

Ts_prod <- function(x,N){
  data1 = vreplace(x)
  datanew = aaply(data1,c(1,3,4),function(x){
    if(sum(!is.na(x))>=N){
      c(RollingProd(x,window=N,na_method="ignore"))
    }else{
      rep(NA,length(x))
    }
  },.drop=FALSE)
  result = aperm(datanew,c(1,4,2,3))
  names(dimnames(result)) = c("K","D","T","V")
  dimnames(result)$D <- dimnames(data1)$D
  return(result)
}

CrossLmResidual <- function(x,y){
  data1 = vreplace(x)
  data2 = vreplace(y)
  datanew = panel.combine(list(data1,data2))
  datanew <- aaply(datanew,c(2,3),function(m){
    NAidx <- is.na(m[,1]) | is.na(m[,2]) | is.infinite(m[,1]) | is.infinite(m[,2])
    res <- .lm.fit(y = m[!NAidx, 2, drop=FALSE],x=m[!NAidx,1,drop=FALSE])$residuals
    out <- rep(NA, length(NAidx))
    out[!NAidx] <- res
    out
  }, .drop =FALSE)
  dimnames(datanew)[[3]] <- dimnames(data1)$K
  datanew <- panel.add.dim(datanew,'V',"tmp")
  datanew <- aperm(datanew,c(3,1,2,4))
  datanew
}

QuantileUpFilter <- function(x,N){
  data1 = vreplace(x)
  datanew <- aaply(data1, c(2,3,4), function(m){
    m[m<quantile(m,N,na.rm=TRUE)] = 0
    m
  },.drop=FALSE)
  datanew <- aperm(datanew,c(4,1,2,3))
  datanew
}

QuantileDownFilter <- function(x,N){
  data1 = vreplace(x)
  datanew <- aaply(data1, c(2,3,4), function(m){
    m[m>quantile(m,N,na.rm=TRUE)] = 0
    m
  },.drop=FALSE)
  datanew = aperm(datanew,c(4,1,2,3))
  datanew
}
