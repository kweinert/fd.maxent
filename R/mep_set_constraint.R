#' Set Constraint in MaxEntProblem
#'
#' If mep_solve was called successfully, use this function to extract the 
#' variables from the solution
#'
#' @param mep an object of class "MaxEntProblem"
#' @param j integer, index of the constraint
#' @param xt integer, value to set
#' @param rhs numerical, right-hand side of the constraint
#' @param indices integer, index/indices of the variables
#' @return an object of class "MaxEntProblem"
#' @export
mep_set_constraint <- function(mep, j, xt, rhs, indices=NULL) {
	if(missing(j)) stop("missing argument 'j'")
	if(is.null(indices)) {
		indices <- which(xt!=0)
		xt <- xt[indices]
	} else {
		if(length(indices)>1 && length(xt)==1) xt <- rep(xt, length(indices))
	}
	
	if(mep$control$storage[1] %in% c("dense", "sparse")) {
		mep$Amat[indices,j] <- xt
	} else { # sqlite	
		new <- data.frame(
			i=indices,
			j=j,
			x=xt
		)
		insertnew <- RSQLite::dbSendQuery(mep$Amat, "INSERT OR REPLACE INTO Amat VALUES (:i,:j,:x);") # if (i,j) exists, delete them first
		RSQLite::dbBind(insertnew, params=new) # execute
		RSQLite::dbClearResult(insertnew)  # release the prepared statement
	}
	mep$bvec[j] <- rhs
	if(mep$control$init[1]=="guess") { # precompute lambda
		Acol <- rep(0, mep$nvar)
		Acol[indices] <- xt
		optfun <- function(lambda) {
			y <- exp(Acol * lambda)
			Z <- sum(y)
			sum(Acol*as.numeric(y))/Z - rhs
		}
		sol <- uniroot(optfun, interval=c(-708, 708/max(Acol)), tol=mep$control$tol) # exp(710)==Inf
		mep$lambda[j] <- sol$root
	}
	mep$support[indices] <- mep$support[indices] + 1
	return(mep)
}

