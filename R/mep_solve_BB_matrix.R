#' Solve Maximum entropy Problem for dense/sparse Matrix using BB
#'
#' Internal function, use mep_solve instead.
#'
#' @param mep an object of class "MaxEntProblem"
#' @param verbose logical, diagnostic messages
#' @return an object of class "MaxEntProblem"
mep_solve_BB_matrix <- function(mep, verbose=TRUE) {
  init <- if(mep$control$init[1]=="runif") runif(mep$ncons) else mep$lambda
  optfun <- function(lambda) {
    y <- exp(mep$Amat %*% lambda)
    Z <- sum(y)
    # browser()
	if(inherits(mep$Amat, "dgCMatrix"))
		Matrix::colSums(mep$Amat*as.numeric(y))/Z - mep$bvec
	else
		colSums(mep$Amat*as.numeric(y))/Z - mep$bvec
  } 
  optres <- BB::BBsolve(par=init, fn=optfun, quiet=FALSE, control=list(tol=mep$control$tol, trace=verbose, triter=1))
  mep$status <- optres$convergence
  if(mep$status==0) {
    mep$prob <- exp(mep$Amat %*% optres$par) # dense / sparse
    mep$prob  <- mep$prob / sum(mep$prob)
    mep$lambda <- optres$par
  }
  return(mep)
}
