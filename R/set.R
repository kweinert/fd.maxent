#' Define a set
#'
#' @param ... elements of a set, must be convertible to character
#' @return an object of class "set"
#' @export
set <- function(...) {
	dots <- list(...)
	if(any(!is.null(names(dots)))) stop("don't use named arguments in set.")
	ret <- unlist(lapply(dots, as.character))
	class(ret) <- c(class(ret), "set")
	return(ret)
}
