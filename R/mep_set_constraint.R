# set constraint j \in {1...ncons}
mep_set_constraint <- function(mep, j, xt, rhs, indices=NULL) {
	if(missing(j)) stop("missing argument 'j'")
	if(is.null(indices)) {
		indices <- which(xt!=0)
		xt <- xt[indices]
	} else {
		if(is.character(indices)) indices <- which(mep$varnames %in% indices) # TODO: test that
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
		insertnew <- dbSendQuery(mep$Amat, "INSERT OR REPLACE INTO Amat VALUES (:i,:j,:x);") # if (i,j) exists, delete them first
		dbBind(insertnew, params=new) # execute
		dbClearResult(insertnew)  # release the prepared statement
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

