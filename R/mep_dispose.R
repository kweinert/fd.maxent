#' Dispose MaxEntProblem
#'
#' Some backends may create a file. Call this function to disconnect from that file.
#'
#' @param mep an object of class "MaxEntProblem"
#' @return NULL
#' @export
mep_dispose <- function(mep) {
	if(mep$control$storage[1]=="sqlite") RSQLite::dbDisconnect(mep$Amat)
	mep <- NULL
}
