mep_solve_BB_matrix <- function(mep, verbose=TRUE) {
  init <- if(mep$control$init[1]=="runif") runif(mep$ncons) else mep$lambda
  optfun <- function(lambda) {
    y <- exp(mep$Amat %*% lambda)
    Z <- sum(y)
    # browser()
    colSums(mep$Amat*as.numeric(y))/Z - mep$bvec
  } 
  optres <- BBsolve(par=init, fn=optfun, quiet=FALSE, control=list(tol=mep$control$tol, trace=verbose, triter=1))
  mep$status <- optres$convergence
  if(mep$status==0) {
    mep$prob <- exp(mep$Amat %*% optres$par) # dense / sparse
    mep$prob  <- mep$prob / sum(mep$prob)
    mep$lambda <- optres$par
  }
  names(mep$prob) <- mep$varnames
  return(mep)
}
