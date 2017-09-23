tp = getTaskPath()
dataPath = tp$rdatapath

inputData <- function(cfg, verbose = FALSE){
  dataName = file.path(dataPath,'dataCombine',cfg$mkt,cfg$freq,cfg$vers,paste(cfg$dtype,cfg$univ,cfg$stDate,
                                                                              cfg$edDate,sep = "_"))
  data = panel.read(dataName,verbose = verbose)
  return(data)
}

genAlp <- function(cfg, loadData = TRUE, verbose = FALSE,envir=.GlobalEnv){
  if(is.null(cfg$expr))stop("no alp expr for cfg!")
  if(is.null(cfg$alpName))cfg$alpName = cfg$expr
  if(is.null(cfg$univ))cfg$univ = "zz800andsmb"
  if(is.null(cfg$univ.ver))cfg$univ.ver = cfg$univ
  if(is.null(cfg$sizeNeutral))cfg$sizeNeutral = TRUE
  if(is.null(cfg$betaNeutral))cfg$betaNeutral = TRUE
  if(is.null(cfg$indNeutral))cfg$indNeutral = TRUE
  if(loadData){
    data = inputData(cfg = cfg)
    Opn = data[,,,"openPrice",drop=FALSE]
    Hgh = data[,,,"highPrice",drop=FALSE]
    Low = data[,,,"lowPrice",drop=FALSE]
    Cls = data[,,,"closePrice",drop=FALSE]
    Pcls = data[,,,"preclosePrice",drop=FALSE]
    Vol = data[,,,"volume",drop=FALSE]
    Amt = data[,,,"amount",drop=FALSE]
    Ret = data[,,,"pctchange",drop=FALSE]
    Turn = data[,,,"turnover",drop=FALSE]
    Adjf = data[,,,"adjFactor",drop=FALSE]
    Tmktcap = data[,,,"totalmktcap",drop=FALSE]
  }
  alp = eval(parse(text = cfg$expr),.func)
  dimnames(alp)[['V']] = cfg$alpName
  # if(cfg$sizeNeutral) alp = sizeNeutral(alp)
  # if(cfg$betaNeutral) alp = betaNeutral(alp)
  # if(cfg$indNeutral) alp = indNeutral(alp)
  return(alp)
}

winsorize <- function(alp){



}

sizeNeutral <- function(alp){
  
  
}

betaNeutral <- function(alp){
  
}

indNeutral <- function(alp){
  
  
}

