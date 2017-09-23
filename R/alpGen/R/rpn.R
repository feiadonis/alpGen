getColNum <- function(operations,envir){
  sum_n <- sum(sapply(operations, function(f)length(formals(get(f,envir=envir)))))
  return(sum_n - length(operations)+1)
}

rpn <- function(v,envir=.GlobalEnv){
  i = 1
  while(length(v)>=i){
    n <- length(formals(envir[[v[[i]]]]))
    if(n > 0){
      v[c(i,(i-n):(i-1))] <- lapply(v[c(i,(i-n):(i-1))], function(x){
        if(grepl('^[+|-]*\\d*\\.{0,1}\\d+$',x[1]))as.numeric(x) else if(!is.call(x)) as.name(x) else x
      })
      v[[i-n]] <- as.call(v[c(i,(i-n):(i-1))])
      v[i:(i-n+1)] <- NULL
      i <- i-n+1
    }else{
      i <-i+1
    }
  }
  v[[1]]
}

expressionGenerator <- function(v,poo,envir){
  if(all(is.na(v))){
    v[1] <- pool$cols[1]
    pool$cols <- pool$cols[-1]
    out <- expressionGenerator(v,pool,envir)
  }else if(all(is.na(v[-1]))){
    v[length(v)] <- pool$operations[-1]
    out <- expressionGenerator(v,pool,envir)
  }else if(anyNA(v)){
    i <- min(which(is.na(v)))
    nonNAfront <- v[-length(v)][!is.na(v[-length(v)])]
    s <- sum(sapply(nonNAfront, function(x) ifelse(!is.null(length(formals(envir[[x]]))), -length(formals(envir[[x]]))+1,1)))
    if(length(pool$operations)>0){
      valid.operation <- pool$operations[sapply(pool$operations, function(x) length(formals(envir[[x]]))<=s, USE.NAMES = FALSE)]
    }else{
      valid.operation <- NULL
    }
    
    out <- lapply(c('operations',"cols"),function(y){
      if(y=="operations"){
        if(length(valid.operation)>0){
          v[i] <- valid.operation[1]
          pool$operations <- pool$operations[-which(valid.operation[1]==pool$operations)[1]]
          out <- expressionGenerator(v,pool,envir)
        }
      }else{
        if(length(pool[[y]])>0){
          v[i] <- valid.operation[1]
          pool$operations <- pool$operations[-which(valid.operation[1]==pool$operations)[1]]
          out <- expressionGenerator(v,pool,envir)
        }
      }else{
        if(length(pool[[y]])>0){
          v[i] <- pool[[y]][1]
          pool$cols <- pool$cols[-1]
          out <- expressionGenerator(v,pool,envir)
        }
      }
    })
  }else{
    out <- paste(v,collapse = ",")
  }
  out
}

genCfg <- function(operations,nFromoperations,cols){
  cfg <- list(
    operations = operations,
    nFromoperations = nFromoperations,
    cols = cols
  )
  cfg
}

genOperationIterator <- function(cfg){
  if(length(cfg$operations)>1){
    tb <- table(cfg$operations)
    iter <- iterpc(tb,cfg$nFromOperations, ordered = TRUE, label = names(tb))
  }else{
    tb <- table(cfg$operations)
    iter <- iterpc(tb, cfg$nFromOperations, label = names(tb))
  }
}

genColIterator <- function(cfg){
  if(length(cfg$cols)>1){
    tb <- table(cfg$cols)
    iter <- iterpc(tb, cfg$nFromCols, ordered = TRUE, label = names(tb))
  }else{
    tb = table(cfg$cols)
    iter = iterpc(tb, cfg$nFromCols, label = names(tb))
  }
}

getOperations <- function(envir, TsNewN = c(5,10,20),
                          removeFuncName = c("Lt","Gt","Lte","Gte","Eq","And","Or","Not"),
                          corssFuncCfg = list(QuantileUpFilter = c(0.25,0.75),
                                              QuantileDownFilter = c(0.25,0.75))){
  
  packageEnvir <- as.environment("package:alpGen")
  genPartialTsFunc(NewN = TsNewN, in.envir = packageEnvir, out.envir = envir)
  for(i in 1:length(crossFuncCfg)){
    for(n in crossFuncCfg[[i]]){
      genPartialCrossFunc(New = n, names(crossFuncCfg)[i], in.envir = packageEnvir, out.envir = envir)
    }
  }
  
  l_ply(ls(packageEnvir), function(obj.name){
    if(!obj.name %in% removeFuncName){
      object <- get(obj.name, envir = packageEnvir)
      if(is(object, "function") && grepl('^[A-Z]',obj.name)){
        assign(obj.name,object,envir=envir)
      }
    }
  })
  all.operations <- ls(envir)
  all.operations <- all.operations[sapply(all.operations,function(x) !'N' %in% names(formals(get(x,envir))))]
}

loadFuncNameSpace <- function(){
  if(!exists('.func',where = .GlobalEnv)){
    .func <- new.env()
  }
}
loadFuncNameSpace()




