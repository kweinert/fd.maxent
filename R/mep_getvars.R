#' Extract Solution from MaxEntProblem
#'
#' If mep_solve was called successfully, use this function to extract the 
#' variables from the solution
#'
#' @param mep an object of class "MaxEntProblem"
#' @param simplify logical, simplify if possible
#' @param verbose logical, diagnostic messages
#' @return when varinfo is set, a data.frame, otherwise a numerical vector
#' @export
mep_getvars <- function(mep, simplify=TRUE, verbose=TRUE) {
	if(mep$status==-1) stop("problem has not been solved.")
	if(mep$status>0) stop("there was an error while solving the problem.")
	if(!is.null(mep[["varinfo"]])) 
		ans <- vec2df(varinfo=mep[["varinfo"]], vec=mep[["prob"]], simplify=simplify)
	else
		ans <- mep[["prob"]]
	return(ans)
}
