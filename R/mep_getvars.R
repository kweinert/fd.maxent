
mep_getvars <- function(mep, verbose=TRUE) {
	if(mep$status==-1) stop("problem has not been solved.")
	if(mep$status>0) stop("there was an error while solving the problem.")
	return(mep$prob)
}
