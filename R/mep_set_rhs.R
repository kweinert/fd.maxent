# set rhs j \in {1...ncons}
mep_set_rhs <- function(mep, rhs, indices=NULL) {
	if(!is.null(indices)) {
		if(max(indices) > mep$ncons) stop("indices larger than ncons.")
		mep$bvec[indices] <- rhs
	} else
		mep$bvec <- rhs
	return(mep)
}

