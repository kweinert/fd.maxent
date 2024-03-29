#' Approximate Entropy
#'
#' Internal function for LP algorithm, use mep_solve instead.
#'
#' @param breaks numerical
#' @param browse logical, stop within function for inspection
#' @param fig plot diagnostic
#' @return a list with fields fun, breaks, and slopes
approx_entropy <- function(breaks=seq(from=.Machine$double.eps, to=1, length.out=4), browse=FALSE, fig=FALSE) {
	stopifnot(all(is.finite(c(breaks,-log(breaks)*breaks)))) 
	stopifnot(all(!is.na(c(breaks,-log(breaks)*breaks)))) 
	if(browse) browser()
	linear_approx <- approxfun(x=breaks, y=-log(breaks)*breaks)
	
	slopes <- (linear_approx(tail(breaks,-1))-linear_approx(head(breaks,-1))) / diff(breaks)
	
	if(fig) {
		entropy <- function(x) -log(x)*x
		
		x <- seq(.Machine$double.eps, 1, length.out=200)
		y_approx <- linear_approx(x)
		y_real <- entropy(x)
		plot(x,y_approx, type="l", xlim=c(from, to), col="darkred", lwd=2, xlab="p", ylab="H(p)")
		lines(x=x, y=y_real, col="darkgreen", lwd=2)
		message(sum(abs(y_approx-y_real)))
	}
	
	return(list(
		fun=linear_approx,
		breaks=breaks,
		slopes=slopes
	))
}