alpir <- function(alpha, fwd, method ="pearson", Nthr = 30){
  data <- panel.combine(list(alpha,fwd))
  alp <- aaply(data,c(2,3),function(x){
    x[which(is.infinite(x),arr.ind = TRUE)] <-NA
    NAidx <- is.na(x[,1]) | is.na(x[,2])
    if(sum(NAidx)<=Nthr){
      ic <- NA
    }else{
      ic <- tryCatch(cor(x[,1],x[,2],use="pairse.complete.obs",method=method),error=function(e)NA)
    }
    ic*sd(x[,2],na.rm=TRUE)
  })
  if(sum(is.na(alp))>length(alp)/4){
    alpmean = NA
    alpIR <- NA
  }else{
    alpmean = abs(mean(alp,na.rm=TRUE))
    alpIR = abs(mean(alp,na.rm=TRUE))/sd(alp,na.rm=TRUE)*sqrt(242)/sqrt(name2value(dimnames(fwd)[['V']])[['v']])
  }
  return(list(alpmean,alpIR,alp))
}

alpturn <- function(alpha,lag, method = "pearson"){
  alpha[is.infinite(alpha)] = NA
  d = dim(alpha)[2]
  alphalag <- alpha[,1:(d-lag),,,drop=FALSE]
  dimnames(alphalag)[['D']] = dimnames(alpha)[['D']][(lag+1):d]
  dimnames(alphalag)[['V']] = paste(dimnames(alpha)[['V']],'lag',sep="_")
  data = panel.combine(list(alpha[,(lag+1):d,,,drop=FALSE],alphalag))
  alpturn = min(1,1-mean(aaply(data,c(2,3),function(x){cor(x[,1],x[,2],use="pairwise.complete.obs",method=method)}),na.rm = T))
  return(alpturn)
}

crossCor <- function(alp1, alp2, method="pearson"){
  dimnames(alp1)[['V']] = "X"
  dimnames(alp2)[['V']] = 'Y'
  data = panel.combine(list(alp1,alp2))
  cc = mean(aaply(data,c(2,3),function(x){cor(x[,1],x[,2],use="pairwise.complete.obs",method=method)}),na.rm=T)
  return(cc)
}

univFilter <- function(alpha,univ){
  data <- panel.combine(list(alpha,univ))
  data <- aaply(data,c(2,3),function(x){
    x[-which(x[,dim(alpha)[4]+1]==1),] = NA
    x[,-dim(x)[2],drop=FALSE]
  },.drop=FALSE)
  data <- aperm(data,c(3,1,2,4))
  return(data)
}
