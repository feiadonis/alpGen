testOperators <- function(testData, operationsFile = "d:\proj\ics\alpGen\R\alpGen\R\operations.R",fastTest = TRUE){
  if(!file.exists(operationsFile))stop("operation file does not exit")
  op.env <- new.env()
  source(operationsFile, local = op.env)
  if(fastTest){
    dta <- testData[sample(1:dim(testData)[1],5),,,,drop=FALSE]
  }else{
    data <- testData
  }
  
  N = 10
  Q = 0.25
  fnames <- ls(op.env)
  fnames <- fnames[grepl(pattern = '^[A-Z]', fnames)]
  for( name in dimnames(testData)$V){
    assign(name,name,.GlobalEnv)
  }
  out.list <- lapply(fnames,function(fname){
    f <- get(fname, envir = op.env)
    if('N' %in% names(formals(f))){
      pn <- length(formals(f))-1
      fml <- rpn(v = c(dimnames(data)$V[1:pn],N,fname),envir = op.env)
      fml2 <- rpn(v = c(dimnames(data)$V[1:pn],Q,fname),envir = op.env)
      alpha <- tryCatch(eval(fml, envir = op.env),error = function(e)NULL)
      alpha2 <- tryCatch(eval(fml2, envir = op.env),error=function(e)NULL)
      if(is.null(alpha) & !is.null(alpha2)){
        alpha <- alpha2
      }
    }else{
      pn = length(formals(f))
      fml <- rpn(v = c(dimnames(data)$V[1:pn],fname),envir = op.env)
      alpha <- tryCatch(eval(fml,envir = op.env),error=function(e)NULL)
    }
    eval.error <- is.null(alpha)
    dim.error <- identical(dim(alpha),dim(data))
    allna.warning <- ifelse(eval.error,TRUE,all(is.na(alpha)))
    data.frame(func.name=fname,eval.error=eval.error,dim.error=dim.error,allna.warning=allna.warning)
  })
  out <- do.call(rbind,out.list)
  if(any(out ==TRUE)){
    idx <- which(out==TRUE,arr.ind=TRUE)[,1]
    idx <- unique(idx)
    warning(paste(paste(out$func.name[idx],collapse = " "),'did not pass the test'))
    warning('check if library is loaded if you use any function from third party library like Rcpp, plyr')
    out[idx,]
  }else{
    print('pass all test')
  }
}