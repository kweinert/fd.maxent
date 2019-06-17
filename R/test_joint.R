test_joint <- function() {
	marginals <- c(oil=0.2, bm=0.15, gas=0.65, wp=0.5)
	cats <- c("T", "F")
	map <- dims2idx(oil=cats, bm=cats, gas=cats, wp=cats)
	
	ctrl <- list(storage="dense", method="BB", init="guess", maxit=25, tol=0.001)
	mep <- mep_make(nvar=length(map)-1, ncons=4, control=ctrl)
	
	mep <- mep_set_constraint(mep, j=1, xt=as.vector(map["T",,,]), rhs=marginals["oil"])
	mep <- mep_set_constraint(mep, j=2, xt=as.vector(map[,"T",,]), rhs=marginals["bm"])
	mep <- mep_set_constraint(mep, j=3, xt=as.vector(map[,,"T",]), rhs=marginals["gas"])
	mep <- mep_set_constraint(mep, j=4, xt=as.vector(map[,,,"T"]), rhs=marginals["wp"])
	
	mep <- mep_solve(mep)
	solution <- c(mep_getvars(mep),0)
	mep_dispose(mep)
	
	joint_prob <- function(x) solution[as.vector(map[x[[1]], x[[2]], x[[3]], x[[4]]])]
	ans <- expand.grid(oil=cats, bm=cats, gas=cats, wp=cats, stringsAsFactors=FALSE)
	ans$prob <- apply(ans, 1, joint_prob)
	browser()
}

test_joint_lp <- function() {
	library(lpSolveAPI)
	marginals <- c(oil=0.2, bm=0.15, gas=0.65, wp=0.5)
	cats <- c("T", "F")
	map <- dims2idx(oil=cats, bm=cats, gas=cats, wp=cats)
	theory <- c(0.01,0.01,0.01,0.23,0.02,0.02,0.04,0.16,0.02,0.02,0.02,0.33,0.02,0.03,0.06,0) # best found so far
	
	entropy <- function(x) -sum(head(x,-1)*log(head(x,-1)))
	is_valid <- function(x) {
		eq <- function(x,y) abs(x-y) < sqrt(.Machine$double.eps)
		ans <- eq(sum(x[as.vector(map["T",,,])]),marginals[1]) &&
			eq(sum(x[as.vector(map[,"T",,])]),marginals[2]) &&
			eq(sum(x[as.vector(map[,,"T",])]),marginals[3]) &&
			eq(sum(x[as.vector(map[,,,"T"])]),marginals[4])  &&
			eq(sum(x),1)
		names(ans) <- NULL
		return(ans)
	}
	
	lprec <- make.lp(0, ncol=length(map)+1)
	lp.control(lprec, sense="max")
	set.objfn(lprec, obj=c(rep(0, length(map)),1)) # max(min p_i)
	
	ncombis <- 8
	add.constraint(lprec, xt=rep(1, ncombis), indices=as.vector(map["T",,,]), type="=", rhs=marginals["oil"])
	add.constraint(lprec, xt=rep(1, ncombis), indices=as.vector(map[,"T",,]), type="=", rhs=marginals["bm"])
	add.constraint(lprec, xt=rep(1, ncombis), indices=as.vector(map[,,"T",]), type="=", rhs=marginals["gas"])
	add.constraint(lprec, xt=rep(1, ncombis), indices=as.vector(map[,,,"T"]), type="=", rhs=marginals["wp"])
	add.constraint(lprec, xt=c(rep(1, length(map)),0), type="=", rhs=1)
	add.constraint(lprec, xt=1, indices=as.vector(map["F", "F", "F", "F"]), type="=", rhs=0) # there must be an option
	for (i in 1:(length(map)-1)) # don't include the last var, as we know it is zero!
		add.constraint(lprec, xt=c(1, -1), indices=c(i, length(map)+1), type=">=", rhs=0) # min p_i
	
	# solve
	status <- solve(lprec)
	if(status!=0) stop(paste("no solution found, error code ", status)) ### no solution? halt
	solution <- get.variables(lprec)[1:length(map)]
	
	# now find a set of lambda that is close to this solution
	stopifnot(!is_valid(solution))
	stopifnot(!is_valid(theory))
	stopifnot(entropy(theory)>entropy(solution))
	
	joint_prob <- function(x) solution[as.vector(map[x[[1]], x[[2]], x[[3]], x[[4]]])]
	ans <- expand.grid(oil=cats, bm=cats, gas=cats, wp=cats, stringsAsFactors=FALSE)
	ans$prob <- apply(ans, 1, joint_prob)
	browser()
}
