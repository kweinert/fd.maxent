test_bin <- function(n_knots=4) {
	ctrl <- list(storage="dense", method="LP", maxit=25, tol=0.001)
	Amat <- matrix(0, ncol=2,nrow=10)
	Amat[1,1] <- 1
	Amat[2,2] <- 1
	mep <- mep_make(nvar=10, ncons=3, control=ctrl)
	mep <- mep_set_constraint(mep, 1, xt=1, rhs=0.5, indices=1)
	mep <- mep_set_constraint(mep, 2, xt=1, rhs=0.2, indices=2)
	mep <- mep_set_constraint(mep, 3, xt=rep(1,10), rhs=1)
	mep <- mep_solve(mep)
	solution <- mep_getvars(mep)
	theory <- c(0.5, 0.2, rep(0.3/8,8))
	n <- 10
	plot(solution, type="l");lines(x=1:n, y=theory, col="darkred")
	legend("topright", legend=sum(abs(solution-theory)))
	browser()
}