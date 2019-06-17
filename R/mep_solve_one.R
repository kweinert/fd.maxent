# internal
# solves for one constraint
mep_solve_one <- function(mep, i, only_lambda=TRUE) {
  ans <- rep(NA, mep$nvar)
  if(mep$control$storage[1]=="sqlite") {
    Acol <- rep(0, mep$nvar)
    dat <- dbGetQuery(mep$Amat, paste(
      "select i,x from Amat",
      "where j=",i
    ))
    Acol[dat[,"i"]] <- dat[,"x"]
  } else Acol <- mep$Amat[,i]
  
  optfun <- function(lambda) {
    y <- exp(Acol * lambda)
    Z <- sum(y)
    ans <- sum(Acol*as.numeric(y))/Z - mep$bvec[i]
    if(is.finite(ans)) return(ans) else return(sign(ans)*.Machine$double.xmax)
  }
  sol <- uniroot(optfun, interval=c(-708, 709/max(Acol)), tol=mep$control$tol) # exp(710)==Inf
  if(only_lambda) {
    return(sol$root)
  } else {
    mep$support <- as.numeric(Acol!=0)
    mep$prob <- exp(Acol * sol$root)
    mep$prob  <- mep$prob / sum(mep$prob)
    mep$lambda <- sol$root
    mep$status <- 0
    return(mep)
  }
}
