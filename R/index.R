#' Index a variable
#'
#' @param x an object of class "Variab"
#' @param ... elements of a set, must be convertible to character
#' @return a 0/1 vector of length length(x)
#' @export
index <- function(x, ...) {
	dots <- list(...)
	if(!"Variab" %in% class(x)) stop("unknown datatype to idx: ", class(x))
	if(length(dots)>0 && any(is.null(names(dots)))) stop("use only named arguments to idx.")
	unknown <- setdiff(names(dots), names(dimnames(x)))
	if(length(unknown)>0) stop("unknown dimension(s): ", paste(unknown, collapse=", "))
	
	# apply as.character if necessary
	dots <- lapply(dots, function(x) if("set" %in% class(x)) x else set(x))
	
	# set parameters to "["
	nd <- length(dim(x))
	idx <- as.list(rep(list(quote(expr = )), nd)) # https://adv-r.hadley.nz/quasiquotation.html
	for (nm in names(dots)) idx[[which(names(dimnames(x))==nm)]] <- dots[[nm]]
	
	# call "["
	arg <- c(list(x), idx) # https://stackoverflow.com/questions/36665492/how-to-combine-two-lists-in-r
	ret <- try(as.vector(do.call(`[`, arg)), silent=TRUE)
	if(inherits(ret, "try-error")) {
		unknown <- c()
		for (nm in names(dots)) {
			add <- setdiff(dots[[nm]], dimnames(x)[[nm]])
			if(length(add)>0) add <- paste(nm, add, sep="$")
			unknown <- c(unknown, add)
		}
		if(length(unknown)>0) stop("unknown variable dimension set element(s): ", paste(unknown, collapse=", "))
		stop(ret) # does this work?
	}
	names(ret) <- NULL
	return(ret)
}

