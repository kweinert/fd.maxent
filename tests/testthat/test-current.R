context("current")

#TODO
test_that("maxwell speed", {
	Amat <- matrix(0, ncol=2,nrow=10)
	Amat[1,1] <- 1
	Amat[2,2] <- 1
	mep <- mep_make(nvar=10, ncons=2, control=list(storage="dense", init="runif"))
	mep <- mep_set_constraint(mep, 1, xt=1, rhs=0.5, indices=1)
	mep <- mep_set_constraint(mep, 2, xt=1, rhs=0.2, indices=2)
	mep <- mep_solve(mep)
	solution <- mep_getvars(mep)
	mep_dispose(mep)
	expect_equivalent(solution, c(0.5, 0.2, rep(0.3/8,8)), tolerance=0.001)
})
