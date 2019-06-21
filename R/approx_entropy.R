approx_entropy <- function(breaks=seq(from=.Machine$double.eps, to=1, length.out=4), fig=FALSE) {

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