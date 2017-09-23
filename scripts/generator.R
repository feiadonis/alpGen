library(icsUtil)
library(plyr)
library(alpGen)
library(iterpc)
library(data.table)
.func <- new.env()
all.operations <- getOperations(.func, TsNewN = c(5,10,20),
                                removeFuncName = c('Lt','Gt','Lte','Gte','Eq','And','Or','Not','Exp'),
                                crossFuncCfg = list(QuantileUpFilter = c(0.25,0.75),
                                                    QuantileDownFilter = c(0.25,0.75)))

all.cols <- c("Cls","Ret","Amt","Vwp",'Turn',"Tshr","Tmcap")
for(name in all.cols){
  assign(name,name,.GlobalEnv)
}

data = panel.read("d:\proj\data\ics\dataCombine\CHINA_STOCK\DAILY\md_smallUniv_20080101_20161231")
fwd1 = panel.read("d:\proj\data\ics\dataCombine\CHINA_STOCK\DAILY\fwd1_smallUniv_20080101_20161231")
fwd5 = panel.read("d:\proj\data\ics\dataCombine\CHINA_STOCK\DAILY\fwd5_smallUniv_20080101_20161231")
fwd10 = panel.read("d:\proj\data\ics\dataCombine\CHINA_STOCK\DAILY\fwd10_smallUniv_20080101_20161231")

data[,,,c('Fashr','Tshr','Tmcap','Fmcap','Frfshr')] <- log(data[,,,c('Fashr','Tshr','Tmcap','Fmcap','Frfshr'),drop=FALSE])
data[,,,c('Vol','Amt')] <- sqrt(data[,,,c('Vol','Amt'),drop=FALSE])

ins <- as.character(getTradingDayRange(20100101,20130101))
data <- data[,ins,,,drop=FALSE]
fwd10 <- fwd10[,ins,,,drop=FALSE]

n.select.op <- 4

good.file <- '~/Desktop/good.txt'
bad.file <- '~/Desktop/bad.txt'

save.path <- "d:\proj\data\ics\ra\CHINA_STOCK\DAILY"
goodtablefile <- file.path(save.path, "goodalp", "goodalp.rds")
if(!file.exists(goodtablefile)){
  goodtable.full = NULL
  goodcount = 0
}else{
  goodtable.full = readRDS(goodtablefile)
  goodcount <- as.integer(substr(goodtable.full[[nrow(goodtable.full),"alpname"]],4,100000000))
}

alpcount <- 0 # use to count alp tested
lastalp = ""
flag = 1 # if 1, pass, if 0, test util lastalp and continue

op.combn.iter <- iterpc(n = 1:length(all.operations),r = n.select,op, labels = all.operations, replace =TRUE)
op.set <- getnext(op.combn.iter)
while(!is.null(op.set)){
  raw.cfg <- genCfg(operations = c(op.set),nFromOperations = length(op.set), cols = all.cols)
  i.op <- 1
  i.col <- 1
  op.permn.iter <- genOperationIterator(raw.cfg)
  op.queue <- getnext(op.permn.iter)
  while(!is.null(op.queue)){
    cfg <- raw.cfg
    cfg$operations <- op.queue
    cfg$nFromCols <- getCoNum(cfg$operations, envir = .func)
    col.permn.iter <- genColIterator(cfg)
    col.queue <- getnext(col.permn.iter)
    while(!is.null(col.queue)){
      cfg$cols <- col.queue
      cfg$i.col <- i.col
      cfg$i.op <- i.op
      pool <- list(operations = op.queue,cols = col.queue)
      res <- lapply(unlist(expressionGenerator(v = rep(NA, length(pool$operations) + length(pool$cols)),pool, envir=.func)),
                    function(x){
                      strsplit(x,',')[[1]]
                    })
      res <- lapply(res,function(x){
        rpn(x,envir= .func)
      })
      
      for(f in res){
        alpcount <- alpcount + 1
        if(deparse(f) == lastalp) flag = 1
        if(!flag)next
        alpha <- eval(f,.func)
        if(all(is.na(alpha))) browser()
        alp = alpir(alpha,fwd10)
        alpmean = alp[[1]]
        alpIR = alp[[2]]
        if(is.na(alpIR)) alpIR = 0
        if(alpIR > 2){
          ac = alpturn(alpha,5)
          print(paste('good alpha', deparse(f)))
          write(paste(deparse(f),'\t',paste(c(alpmean,alpIR,ac),collapse = '\t')),good.file.append=TRUE)
          if(is.na(ac))ac=1
          if(ac < 0.65){
            # good alpha, test alp correlation
            alpname = 'alpnew'
            dimnames(alpha)[['V']] = alpname
            alpha[is.infinite(alpha)] = NA
            alpha = standardize(alpha)
            thistable <- data.table(alpname = alpname,expr = deparse(f), alpmean = alpmean, alpIR = alpIR, turnover = ac,
                                    alpcor = 0, status = 1, replaced = "NULL")
            if(is.null(goodtable.full)){
              #change alp name
              goodcount <- goodcount + 1
              alpname <- paste0('alp', goodcount)
              dimnames(alpha)[['V']] = alpname
              filename = file.path(save.path, 'goodalp', paste(alpname,'.h5',sep=""))
              panel.write(alpha, filename, overwrite = T)
              thistable$alpname = alpname
              goodtable.full = thistable
              saveRDS(goodtable.full, file = file.path(save.path, "goodalp", "goodalp.rds"))
            }else{
              pregoodalps <- goodtable.full$alpname
              tmpcor <- lapply(pregoodalps, function(gdalp){
                gdalppath <- file.path(save.path, "goodalp", paste0(gdalp,".h5"))
                if(!file.exists(gdalppath)){
                  #generate it
                  gdexpr = goodtable.full[alpname == gdalp,expr]
                  gdalpdata = eval(parse(text = gdexpr),.func)
                  gdalpdata = standardize(gdalpdata)
                  dimnames(gdalpdata)[['V']] = gdalp
                  panel.write(gdalpdta,gdalppath)
                }else{
                  gdalpdata <- panel.read(gdalppath)
                }
                alpdata <- panel.combine(list(alpha, gdalpdata))
                alpcor <- aaply(aaply(alpdata,c(2,3),cor,use="pairwise.complete,obs"),c(2,3),mean,na.rm=TRUE)
                paircor <- alpcor[2,1]
                paircor
              })
              highcoralp <- pregoodalps[which(tmpcor > 0.7)]
              if(!is.null(highcoralp) && length(highcoralp)!=0){
                alpIRs <- goodtable.full(alpname %in% highcoralp, alpIR)
                if(alpIR > max(alpIRs)){
                  # then we replace
                  goodcount = goodcount + 1
                  alpname = paste0('alp', goodcount)
                  dimnames(alpha)[['V']] = alpname
                  goodtable.full[alpname %in% highcoralp,"status"] = 0
                  goodtable.full[alpname %in% highcoralp,"replaced"] = alpname
                  thistable$alpname = alpname
                  goodtable.full = rbind(goodtable.full, thistable)
                  goodtable.full = goodtable.full[status == 1]
                  
                  # update alp data
                  filename = file.path(save.path, "goodalp", paste0(alpname, ".h5"))
                  panel.write(alpha, filename, overwrite = T)
                  gc()
                  saveRDS(goodtable.full, file = file.path(save.path,"goodalp/goodalp.rds"))
                  
                  # delete alpha be replaced
                  llply(highcoralp, function(halp){
                    alppath <- file.path(save.path, 'goodalp', paste0(halp,".h5"))
                    print(paste("rm",alppath))
                    system(paste("rm",alppath))
                  })
                }
              }else{
                goodcount = goodcount + 1
                alpname = paste0("alp",goodcount)
                dimnames(alpha)[['V']] = alpname
                thistable$alpname = alpname
                goodtable.full = rbind(goodtable.full,thistable)
                goodtable.full = goodtable.full[status == 1]
                filename = file.path(save.path,'goodalp',paste0(alpname,'.h5',sep=""))
                panel.write(alpha, filename,overwrite = T)
                gc()
                saveRDS(goodtable.full,file = file.path(save.path, 'goodalp/goodalp.rds'))
              }
            }
          }
        }else{
          print(paste('bad alpha', alpcount, deparse(f)))
          write(paste(alpcount, deparse(f), alpmean, alpIR, sep="\t"), bad.file, append = T)
        }
      }
      i.col <- i.col + 1
      col.queue <- getnext(col.permn.iter)
    }
    i.op <- i.op + 1
    op.queue <- getnext(op.permn.iter)
  }
  op.set <- getnext(op.combn.iter)
}




