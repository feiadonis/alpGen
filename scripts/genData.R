library(icsUtil)
library(plyr)
library(abind)

dataPath = getTaskPath()$rdatapath
getData <- function(stDate,edDate, dtype = "md", mkt = "CHINA_STOCK", freq = "DAILY", vers = "wind",
                    univ = "zz800andsmb", univ.ver = univ){
  td = getTradingDayRange(stDate,edDate)
  data = readicsdata(td, dtype = dtype, mkt = mkt, freq = freq, vers = vers)
  dnames = dimnames(data)[['V']]
  dataUniv = readicsdata(td, dtype="univ", mkt = mkt, freq = freq, vers = univ.ver)
  dataUniv = dataUniv[,,,univ,drop=FALSE]
  datanew = panel.combine(list(data,dataUniv))
  td = dimnames(datanew)[["D"]]
  tmplist = llply(td, function(t){
    tmpdata = datanew[,t,,,drop=FALSE]
    tmpdata = tmpdata[which(tmpdata[,t,1,univ]==1),t,1,dnames,drop=FALSE]
    tmpdata
  })
  dataCombine = panel.combine(tmplist)
  resultPath = file.path(dataPath,'dataCombine',mkt,freq,vers)
  if(!dir.exists(resultPath))dir.create(resultPath,recursive = T)
  resultName = file.path(resultPath,paste(dtype,univ,stDate,edDate,sep="_"))
  panel.write(dataCombine,filename = resultName,overwrite = T)
}
