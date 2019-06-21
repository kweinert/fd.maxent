mep_dispose <- function(mep) {
	if(mep$control$storage[1]=="sqlite") dbDisconnect(mep$Amat)
	mep <- NULL
}
