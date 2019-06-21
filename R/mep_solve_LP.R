mep_solve_LP <- function(mep, verbose=TRUE) {
	
	n_knots <- 4
	n_var <- mep[["nvar"]]
	n_cons <- mep[["ncons"]]
	b_vec <- mep[["bvec"]]
	best_prob <- rep(1/length(n_var), n_var)
	best_entr <- sum(-log(best_prob)*prob)
	
	# approximate entropy
	knot_info <- lapply(rep(n_knots, n_var), FUN=function(x) approx_entropy(
		breaks=seq(from=.Machine$double.eps, to=1, length.out=n_knots)
	))
	
	# objective
	lprec <- make.lp(nrow=n_cons+n_var*(n_knots-1)+n_var, ncol=n_var*(n_knots-1)+1)
	lp.control(lprec, sense="max")
	obj <- sapply(knot_info, function(x) x[["slopes"]])
	dim(obj) <- NULL
	set.objfn(lprec, obj=c(obj,10)) # objective function
	if(verbose) message("lp objective set.")
	
	# constraint types
	set.constr.type(
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
	set.rhs(lprec, b=c(b_vec, rhs, rep(0, times=n_var)))
	if(verbose) message("rhs set.")
	
	# mep constraints
	for (j in seq(n_cons)) set.row(lprec, row=j, xt=c(rep(mep$Amat[,j], each=(n_knots-1)),0))
	if(verbose) message("mep constraints transfered to lp.")
	
	# segment boundaries
	for (j in seq(n_var*(n_knots-1))) set.row(lprec, row=n_cons+j, xt=1, indices=j)
	if(verbose) message("segment boundaries contraints added to lp.")

	# try to be > 0
	for (j in seq(n_var)) {
		set.row(
			lprec, 
			row=n_cons+n_var*(n_knots-1)+j, 
			xt=c(rep(1, n_knots-1), -1), 
			indices=c(seq((j-1)*(n_knots-1)+1, length.out=n_knots-1), n_var*(n_knots-1)+1)
		) # min p_i
	}
	if(verbose) message("additional variable min p_i defined.")
	if(verbose) message("initial lp set up.")

	# helper function to find new knots
	calc_breaks <- function(ki, sol) {
		new_to <- ki$breaks[sum(sol>0)+1]
		if(new_to<ki$breaks[n_knots]) 
			seq(from=ki$breaks[1], to=new_to, length.out=n_knots)
		else
			c(.Machine$double.eps, seq(from=ki$breaks[2], to=new_to, length.out=n_knots-1))
	}
	
	# narrow the intervals 
	iter <- 1
	old_entropy <- 0
	old_prob_summed <- rep(NA, length(n_var))
	repeat {
		# solve first
		status <- solve(lprec)
		if(status!=0) {
			warning(paste("no solution found, error code ", status)) ### no solution? exit
			mep$status <- status
			return(mep)
		}
		
		# examine solution
		prob <- head(get.variables(lprec), -1)
		prob_sliced <- slice(prob, n_knots-1)
		prob_summed <- sapply(prob_sliced, FUN=sum)
		entropy <- sum(-log(prob_summed)*prob_summed)
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
			breaks <- calc_breaks(knot_info[[i]], prob_sliced[[i]])
			knot_info[[i]] <- approx_entropy(breaks=breaks)
		}
		if(verbose) message("new breaks calculated.")
		
		# new objective
		obj <- sapply(knot_info, function(x) x[["slopes"]])
		dim(obj) <- NULL
		gap <- best_entr - entropy
		set.objfn(lprec, obj=c(obj,100)) # objective function
		if(verbose) message("objective updated.")
		  
		# new rhs
		rhs <- sapply(knot_info, function(x) x$breaks)
		rhs <- apply(rhs, 2, diff)
		set.rhs(
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
