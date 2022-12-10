#' Balance a dataset
#' 
#' See vignette("balance") for more details.
#'
#' @param svy a dataset to calculate weights for
#' @param pop a dataset representing the whole population
#' @param structvars character column(s) to balance for
#' @return a numeric vector of length nrow(svy)
#' @export
balance <- function(svy, pop, structvars) {
	ncons <- 1
	for (x in structvars) {
		if(!x %in% colnames(svy)) stop("missing column in svy: ", x)
		if(!x %in% colnames(pop)) stop("missing column in pop: ", x)
		ncons <- ncons + length(unique(pop[,x]))-1
	}
		
	# set up mep
	nvars <- nrow(svy)
	mep <- mep_make(	
		nvar=nvars, 
		ncons=ncons, 
		control=list(method=if(nvars>40) "BB" else "LP")
	)
	
	# constraints
	j <- 1
	for (x in structvars) {
		props <- table(pop[,x]) |> proportions()
		for (y in head(unique(pop[,x]), -1)) {
			idx <- which(svy[,x]==y)
			mep <- mep_set_constraint(mep, j=j, xt=1, indices=idx, rhs=props[y])
			j <- j + 1
		}
	}
	mep <- mep_set_constraint(mep, j=j, xt=rep(1, nvars), indices=seq(1, nvars), rhs=1)
	
	# solve
	mep <- mep_solve(mep, verbose=FALSE)
	if(mep[["status"]]!=0) stop("could calculate balance weights")
	solution <- as.vector(mep_getvars(mep))

	return(solution*nvars)
}

