# tf <- set("T", "F")
# fuels <- set("oil", "bm", "gas", "wp")
# x <- variab(fuel=fuels, avail=tf)
# index(x, fuel="bm", avail="T")
set <- function(...) {
	dots <- list(...)
	if(any(!is.null(names(dots)))) stop("don't use named arguments in set.")
	ret <- unlist(lapply(dots, as.character))
	class(ret) <- c(class(ret), "set")
	return(ret)
}
	
variab <- function(..., browse=FALSE) {
	dots <- list(...)
	if(any(is.null(names(dots)))) stop("only use named arguments in variab.")
	dots <- lapply(dots, function(x) if("set" %in% class(x)) x else set(x))
	dims <- sapply(dots, length)
	n <- prod(dims)
	dim_nm <- lapply(dots, as.character)
	# names(dim_nm) <- NULL
	ret <- array(seq(1,n), dim=dims, dimnames=dim_nm)
	if(browse) browser()
	class(ret) <- c(class(ret), "variab")
	return(ret)
}

index <- function(x, ...) {
	dots <- list(...)
	if(!"variab" %in% class(x)) stop("unknown datatype to idx: ", class(x))
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

vec2df <- function(x, v) {
	if(!"variab" %in% class(x)) stop("unknown datatype to idx: ", class(x))
	if(length(x)!=length(v)) stop("differing length between index and vector: ", length(x), length(vec))
	
	arg <- dimnames(x)
	arg[["stringsAsFactors"]]=FALSE
	ret <- do.call(expand.grid, arg)
	val <- rep(NA, nrow(ret))
	lookup <- function(...) index(x, ...)
	for (i in seq(nrow(ret))) {
		arg <- unlist(ret[i,, drop=TRUE])
		val[i] <- v[do.call(lookup, as.list(arg))]
	}
	ret[,"value"] <- val
	return(ret)
}
