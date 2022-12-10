#' Define a variable
#'
#' @param ... elements of a set, must be convertible to character
#' @return an object of class "Variab"
#' @export
variab <- function(...) {
	dots <- list(...)
	if(any(is.null(names(dots)))) stop("only use named arguments in variab.")
	dots <- lapply(dots, function(x) if("set" %in% class(x)) x else set(x))
	dims <- sapply(dots, length)
	n <- prod(dims)
	dim_nm <- lapply(dots, as.character)
	# names(dim_nm) <- NULL
	ret <- array(seq(1,n), dim=dims, dimnames=dim_nm)
	class(ret) <- c(class(ret), "Variab")
	return(ret)
}
