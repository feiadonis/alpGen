genPartialTsFunc <- function(NewN, in.envir = .GlobalEnv, out.envir = .GlobalEnv){
  for(fname in ls(in.envir)){
    f <- get(fname, envir = in.envir)
    if(is.function(f) && 'N' %in% names(formals(f)) && grepl('^Ts|^TS',fname)){
      for(n in NewN){
        assign(paste0(fname,"_",n),partialFunction(f,fname,N = n), envir = out.envir)
      }
    }
  }
  return(TRUE)
}

genPartialCrossFunc <- function(NewN, fname, in.envir = .GlobalEnv, out.envir = .GlobalEnv){
  if(fname %in% ls(in.envir)){
    f <- get(fname, envir = in.envir)
    if(is.function(f) && 'N' %in% names(formals(f)) && !grepl('^Ts|^TS',fname)){
      for(n in NewN){
      assign(paste0(fname,'_',n),partialFunction(f,fname,N=n),envir = out.envir)
      }
    }
    return(TRUE)
  }else{
    return(FALSE)
  }
}

vreplace <- function(x){
  y <- ifelse(is.array(x)|| is.numeric(x), x, data[,,,x,drop=FALSE])
  return(y)
}