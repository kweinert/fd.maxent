# N. Agmon, Y. Alhassid, R. D. Levine, An algorithm for finding the distribution of maximal entropy, Journal of Computational Physics,30 , 250-258, 1979
# A. Golan, G. Judge, D. Miller, Maximum entropy econometrics: robust estimation with limited data, Wiley, New York, 1996.
# B. Chen, J. Hu, Y.Zhu, Computing maximum entropy densities: A hybrid approach, Signal Processing: An International Journal, 4(2), 114-122, 2010.
# D. Ormoneit, H. White, An efficient algorithm to compute maximum entropy densities, Econometrics reviews, 18(2), 127-140, 1999.
# Aharon Ben-Tal, Arkadi Nemirovski: Lectures on Modern Convex Optimizsation

mep_make <- function(nvar, ncons, varnames=NULL, control=list()) {
	ctrl <- list(storage=c("dense", "sparse", "sqlite"), method=c("BB"), init=c("runif", "guess"), breaks=5, maxit=25, tol=.Machine$double.eps^0.25, dbname="")
	diffctrl <- setdiff(names(control), names(ctrl))
	if(length(diffctrl)>0) stop("unknown control name(s): ", paste(diffctrl, collapse=", "))
	if(!is.null(varnames) && length(varnames)!=nvar) stop("nvar and length(varnames) do not match.")
	for (n in names(control)) ctrl[[n]] <- control[[n]]
	ans <- list(nvar=nvar, ncons=ncons, Amat=NULL, bvec=NULL, support=NULL, prob=NULL, lambda=NULL, control=ctrl, status=-1) # -1 unsolved
	if(ans$control$storage[1]=="sqlite") {
		if(ans$control$dbname=="") ans$control$dbname <- tempfile(fileext=".db")
		ans$Amat <- dbConnect(RSQLite::SQLite(), dbname=ans$control$dbname)
		# initExtension(ans$Amat) # for exp
		dbExecute(ans$Amat, "CREATE TABLE IF NOT EXISTS Amat (i INTEGER NOT NULL, j INTEGER NOT NULL, x DOUBLE NOT NULL, PRIMARY KEY (i, j));")
		dbExecute(ans$Amat, "CREATE INDEX IF NOT EXISTS idx_Amat on Amat(i, j);")
	} else if(ans$control$storage[1]=="dense") 
		ans$Amat <- matrix(0, nrow=nvar, ncol=ncons)
	else if(ans$control$storage[1]=="sparse") 
		ans$Amat <- sparseMatrix(i=1, j=1, x=0, dims=c(nvar, ncons))
	else stop("unknown storage method: ", ans$control$storage)
	ans$bvec <- rep(NA, ncons)
	ans$varnames <- varnames
	ans$lambda <- rep(NA, ncons)
	ans$support <- rep(0, nvar)
	ans$prob <- rep(NA, nvar)
	class(ans) <- "MaxEntProblem"
	return(ans)
}

