#' Index a variable
#'
#' @param varinfo an object of class "Variab"
#' @param vec a numeric vector
#' @param simplify logical, simplify result if possible
#' @return a data.frame
#' @export
vec2df <- function(varinfo, vec, simplify=TRUE) {
	if(!"Variab" %in% class(varinfo)) stop("unknown datatype to idx: ", class(varinfo))
	if(length(varinfo)!=length(vec)) stop("differing length between index and vector: ", length(varinfo), length(vec))
	is_onedim <- length(dim(varinfo))==1
	
	if(is_onedim && simplify) {
		ret <- setNames(vec,dimnames(varinfo)[[1]])
	} else {
		arg <- dimnames(varinfo)
		arg[["stringsAsFactors"]] <- FALSE
		ret <- do.call(expand.grid, arg)
		val <- rep(NA, nrow(ret))
		lookup <- function(...) {index(varinfo, ...)}
		for (i in seq(nrow(ret))) {
			arg <- unlist(ret[i,, drop=TRUE])
			if(is_onedim) names(arg) <- names(dimnames(varinfo))
			val[i] <- vec[do.call(lookup, as.list(arg))]
		}
		ret[,"value"] <- val
	}
	return(ret)
}
