ctrl <- list(storage="dense", method="BB", init="runif", maxit=25, tol=.Machine$double.eps^0.25)
n=40
p=0.3
theory <- dbinom(1:n, size=n, prob=p)
mu <- n*p
mep <- mep_make(nvar=n, ncons=1, control=ctrl)
expect_error(mep_set_constraint(mep, xt=(1:n-mu)^2, rhs=n*p*(1-p)))
mep <- mep_set_constraint(mep, j=1, xt=(1:n-mu)^2, rhs=n*p*(1-p))
mep <- mep_solve(mep)
solution <- mep_getvars(mep)
mep_dispose(mep)
expect_equal(solution, theory, tolerance=0.05)

ctrl <- list(storage="dense", method="BB", init="runif", maxit=25, tol=.Machine$double.eps^0.25)
mep <- mep_make(nvar=10, ncons=2, control=ctrl)
mep <- mep_set_constraint(mep, 1, xt=1, rhs=0.5, indices=1)
mep <- mep_set_constraint(mep, 2, xt=1, rhs=0.2, indices=2)
mep <- mep_solve(mep)
solution <- mep_getvars(mep)
mep_dispose(mep)
expect_equivalent(as.vector(solution), c(0.5, 0.2, rep(0.3/8,8)), tolerance=0.001)

