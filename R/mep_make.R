#' Creates an S3 object for specifications of a Maximum Entropy Problem
#' 
#' You need to specify the number of variables and the number of constraints beforehand. 
#' The constraints itself are set using mep_set_constraint.
#' Optionally, you can provide variable names. The control argument allows to specify the storage mode
#' (storage, either "dense", "sparse" or "sqlite"), the solving algorithm (method, either "BB" or "LP"), 
#' the maximum number of iterations (maxit, default 25) and the tolerance (tol, default .Machine$double.eps^0.25) and,
#' in case of storage="sqlite", the name of the database file (dbname, default tempfile()).
#'
#' @param nvar the number of variables
#' @param ncons the number of constraints
#' @param varinfo optional, a character vector of length(nvar) specifing the variable names
#' @param control optional, named list of control variables. See details.
#' @return an S3 object of class MaxEntProblem
#' @export
mep_make <- function(nvar, ncons, varinfo=NULL, control=list()) {
	ctrl <- list(
		storage=c("dense", "sparse", "sqlite"), 
		method=c("BB", "LP"), 
		init=c("runif", "guess"), 
		maxit=25, 
		tol=.Machine$double.eps^0.25, 
		n_knots=4, 
		dbname=""
	)
	diffctrl <- setdiff(names(control), names(ctrl))
	if(length(diffctrl)>0) stop("unknown control name(s): ", paste(diffctrl, collapse=", "))
	if(!is.null(varinfo) && length(varinfo)!=nvar) stop("nvar and length(varinfo) do not match.")
	for (n in names(control)) ctrl[[n]] <- control[[n]]
	if(!"tol" %in% names(control) && ctrl[["method"]][1]=="LP") ctrl[["tol"]] <- 0.00001
	ans <- list(Amat=NULL, bvec=NULL, support=NULL, prob=NULL, lambda=NULL, status=-1) # -1 unsolved
	ans[["nvar"]] <- nvar 
	ans[["ncons"]] <- ncons  
	ans[["control"]] <- ctrl
	if(ans$control$storage[1]=="sqlite") {
		if(ans$control$dbname=="") ans$control$dbname <- tempfile(fileext=".db")
		ans$Amat <- dbConnect(RSQLite::SQLite(), dbname=ans$control$dbname)
		dbExecute(ans$Amat, "CREATE TABLE IF NOT EXISTS Amat (i INTEGER NOT NULL, j INTEGER NOT NULL, x DOUBLE NOT NULL, PRIMARY KEY (i, j));")
		dbExecute(ans$Amat, "CREATE INDEX IF NOT EXISTS idx_Amat on Amat(i, j);")
	} else if(ans$control$storage[1]=="dense") 
		ans$Amat <- matrix(0, nrow=nvar, ncol=ncons)
	else if(ans$control$storage[1]=="sparse") 
    ans$Amat <- sparseMatrix(i=1, j=1, x=0, dims=c(nvar, ncons))
	else stop("unknown storage method: ", ans$control$storage)
  ans$bvec <- rep(NA, ncons)
  ans$varinfo <- varinfo
  ans$lambda <- rep(NA, ncons)
  ans$support <- rep(0, nvar)
  ans$prob <- rep(NA, nvar)
  class(ans) <- "MaxEntProblem"
  return(ans)
}

