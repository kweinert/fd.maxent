#' Solve a MaXEnt problem
#' 
#' Calculates the maximum entropy distribution given the constraints.
#'
#' @param mep an S3 object of class MaxEntProblem. See mep_make
#' @param verbose if TRUE, extra messages are printed
#' @return an S3 object of class MaxEntProblem
#' @export
mep_solve <- function(mep, verbose=TRUE) {
	if(mep$ncons==1) 
		return(mep_solve_one(mep, 1, only_lambda=FALSE))
	else if(mep$control$method[1]=="BB" && mep$control$storage[1]=="sqlite") 
      return(mep_solve_BB_sqlite(mep, verbose=verbose))
	else if(mep$control$method[1]=="BB" && mep$control$storage[1] %in% c("dense", "sparse"))
	    return(mep_solve_BB_matrix(mep, verbose=verbose))
	else stop("niy")
}
