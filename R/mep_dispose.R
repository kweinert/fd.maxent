mep_dispose <- function(mep) {
	if(mep$control$storage=="sqlite") dbDisconnect(mep$Amat)
	mep <- NULL
}
