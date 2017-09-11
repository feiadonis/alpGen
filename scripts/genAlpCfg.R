cfg = list(
  stDate = "20140101",
  edDate = "20160101",
  dtype = "md",
  mkt = "CHINA_STOCK",
  freq = "DAILY",
  vers = "wind",
  univ = "zz800andsmb",
  alpName = "alp1", # if alpName = NULL, then alpName  is the expr
  expr = "add(Opn,Cls)",
  sizeNeutral = TRUE,
  betaNeutral = TRUE,
  indNeutral = TRUE,
  standardize = TRUE
)

if(T){
  a = genAlp(cfg = cfg,loadData = T) # if we do not want load freqently, we should first load data and set parm to F
}

