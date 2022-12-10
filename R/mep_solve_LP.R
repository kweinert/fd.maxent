#' Solve Maximum entropy Problem for dense/sparse Matrix using Linear Programming
#'
#' Internal function, use mep_solve instead.
#'
#' @param mep an object of class "MaxEntProblem"
#' @param verbose logical, diagnostic messages
#' @return an object of class "MaxEntProblem"
mep_solve_LP <- function(mep, verbose=TRUE) {
	n_knots <- mep$control$n_knots
	n_var <- mep[["nvar"]]
	n_cons <- mep[["ncons"]]
	b_vec <- mep[["bvec"]]
	minp <- 0
	best_prob <- rep(1/length(n_var), n_var)
	best_entr <- sum(-log(best_prob)*best_prob)
	
	# approximate entropy
	knot_info <- lapply(rep(n_knots, n_var), FUN=function(x) approx_entropy(
		breaks=seq(from=.Machine$double.eps, to=1, length.out=n_knots)
	))
	
	# objective
	lprec <- lpSolveAPI::make.lp(nrow=n_cons+n_var*(n_knots-1)+n_var, ncol=n_var*(n_knots-1)+1)
	lpSolveAPI::lp.control(lprec, sense="max")
	obj <- sapply(knot_info, function(x) x[["slopes"]])
	dim(obj) <- NULL
	lpSolveAPI::set.objfn(lprec, obj=c(obj,minp)) # objective function
	if(verbose) message("lp objective set.")
	
	# constraint types
	lpSolveAPI::set.constr.type(
		lprec, 
		types=c(
			rep("=", times=n_cons), 
			rep("<=", times=n_var*(n_knots-1)), 
			rep(">=", times=n_var)
		)
	)
	if(verbose) message("constraint types set.")
	
	# rhs 
	rhs <- sapply(knot_info, function(x) x$breaks)
	rhs <- apply(rhs, 2, diff)
	lpSolveAPI::set.rhs(lprec, b=c(b_vec, rhs, rep(.Machine$double.eps*10, times=n_var)))
	if(verbose) message("rhs set.")
	
	# mep constraints
	for (j in seq(n_cons)) 
		lpSolveAPI::set.row(lprec, row=j, xt=c(rep(mep$Amat[,j], each=(n_knots-1)),0))
	if(verbose) message("mep constraints transfered to lp.")
	
	# segment boundaries
	for (j in seq(n_var*(n_knots-1))) lpSolveAPI::set.row(lprec, row=n_cons+j, xt=1, indices=j)
	if(verbose) message("segment boundaries contraints added to lp.")

	# try to be > 0
	for (j in seq(n_var)) {
		lpSolveAPI::set.row(
			lprec, 
			row=n_cons+n_var*(n_knots-1)+j, 
			xt=c(rep(1, n_knots-1)), 
			indices=c(seq((j-1)*(n_knots-1)+1, length.out=n_knots-1))
		) # min p_i
	}
	if(verbose) message("additional variable min p_i defined.")
	if(verbose) message("initial lp set up.")

	# helper function to find new knots
	calc_breaks <- function(ki, sol, browse=FALSE) {
		new_to <- ki$breaks[max(c(sum(sol>0)+1,2))]
		ans <- if(new_to<ki$breaks[n_knots]) 
			seq(from=ki$breaks[1], to=new_to, length.out=n_knots)
		else 
			c(.Machine$double.eps, seq(from=ki$breaks[3], to=new_to, length.out=n_knots-1))
		if(browse) browser()
		return(ans)
	}
	
	# narrow the intervals 
	iter <- 1
	old_entropy <- 0
	old_prob_summed <- rep(NA, n_var)
	repeat {
		# solve first
		status <- lpSolveAPI::solve.lpExtPtr(lprec)
		if(status!=0) {
			warning(paste("no solution found, error code ", status)) ### no solution? exit
			mep$status <- status
			return(mep)
		}
		
		# examine solution
		prob <- head(lpSolveAPI::get.variables(lprec), -1)
		prob_sliced <- slice(prob, n_knots-1)
		prob_summed <- sapply(prob_sliced, FUN=sum)
		idx <- which(prob_summed>0)
		entropy <- sum(-log(prob_summed[idx])*prob_summed[idx])
		if(verbose) message("entropy=", entropy)
		
		# finished?
		if(iter>=mep$control$maxit) {
			warning("reached maxit; no convergence")
			break
		}
		if(!is.finite(entropy)) {
			warning("approaching unstable solution with zero probabilities.")
			prob_summed <- old_prob_summed
			break
		}
		if(abs(entropy-old_entropy)<mep$control$tol) { 
			if(verbose) message("convergence up to tolerance.")
			break
		} 
		iter <- iter + 1 
		old_entropy <- entropy
		old_prob_summed <- prob_summed
		
		# new breaks
		for (i in seq(n_var)) {
			breaks <- calc_breaks(knot_info[[i]], prob_sliced[[i]], browse=FALSE)
			knot_info[[i]] <- approx_entropy(breaks=breaks, browse=FALSE)
		}
		if(verbose) message("new breaks calculated.")
		
		# new objective
		obj <- sapply(knot_info, function(x) x[["slopes"]])
		dim(obj) <- NULL
		gap <- best_entr - entropy
		lpSolveAPI::set.objfn(lprec, obj=c(obj,minp)) # objective function
		if(verbose) message("objective updated.")
		  
		# new rhs
		rhs <- sapply(knot_info, function(x) x$breaks)
		rhs <- apply(rhs, 2, diff)
		lpSolveAPI::set.rhs(
			lprec, b=rhs,
			constraints=seq(length(b_vec)+1, length.out=n_var*(n_knots-1))
		)
		if(verbose) message("rhs updated.")
	}
	if(verbose) message("finished.")
	
	mep$status <- status
	mep$prob <- prob_summed
	return(mep)
}
