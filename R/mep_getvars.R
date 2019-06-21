
mep_getvars <- function(mep, simplify=TRUE, verbose=TRUE) {
	if(mep$status==-1) stop("problem has not been solved.")
	if(mep$status>0) stop("there was an error while solving the problem.")
	if(!is.null(mep[["varinfo"]])) 
		ans <- vec2df(varinfo=mep[["varinfo"]], vec=mep[["prob"]], simplify=simplify)
	else
		ans <- mep[["prob"]]
	return(ans)
}
