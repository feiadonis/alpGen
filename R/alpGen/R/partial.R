partialFunction <- function(`f`, funcName, ...){
  stopifnot(is.function(`f`))
  fcall <- substitute(`f`(...))
  n <- length(formals(`f`))-1
  fcall[[1]] <- as.name(funcName)
  fcall[[length(fcall)]] <- list(...)$N
  for(i in 1:n){
    fcall[[length(fcall)+1]] <- as.name(names(formals(`f`)[i]))
  }
  
  args <- list(`...` = quote(expr = ))
  out <- makeFunction(args, fcall)
  formals(out) <- formals(`f`)[names(formals(`f`))!='N']
  out
}


makeFunction <- function(args, body){
  args <- as.pairlist(args)
  stopifnot(
    allNamed(args),
    is.language(body))
  eval(call('function',args,body),parent.frame())
}

allNamed <- function(x){
  if(length(x) == 0) return(TRUE)
  !is.null(names(x)) && all(names(x) != "")
}