approx_entropy <- function(breaks=c(seq(.Machine$double.eps, 0.01, length.out=10), seq(0.013, 0.05, length.out=10), seq(0.06, 0.368, length.out=10), 1), fig=FALSE) {
	if(is.numeric(breaks))
		if((length(breaks)==1)) {
			stopifnot(breaks>2)
			breaks <- if(breaks==3)
				c(.Machine$double.eps,0.367878,1)
			else if(breaks==4)
				c(.Machine$double.eps,0.135334, 0.367878,1)
			else
				c(log(seq(exp(.Machine$double.eps),exp(0.367878), length.out=breaks-1)), 1)
		} else {
			# browser()
			stopifnot(
				length(breaks)>2,
				min(breaks)>0,
				max(breaks)<=1,
				all(diff(breaks)>0)
			)
		}
	else stop("breaks must be a integer or a numeric vector")
		
	linear_approx <- approxfun(x=breaks, y=-log(breaks)*breaks)
	slopes <- (linear_approx(tail(breaks,-1))-linear_approx(head(breaks,-1))) / diff(breaks)
	
	if(fig) {
		entropy <- function(x) -log(x)*x
		
		x <- seq(.Machine$double.eps, 1, length.out=200)
		y_approx <- linear_approx(x)
		y_real <- entropy(x)
		plot(x,y_approx, type="l", xlim=c(0,0.1), col="darkred", lwd=2, xlab="p", ylab="H(p)")
		lines(x=x, y=y_real, col="darkgreen", lwd=2)
		message(sum(abs(y_approx-y_real)))
	}
	return(list(
		fun=linear_approx,
		breaks=breaks,
		slopes=slopes
	))
}