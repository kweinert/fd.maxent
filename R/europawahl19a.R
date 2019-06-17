europawahl19 <- function(
	breaks=c(seq(.Machine$double.eps, 0.01, length.out=10), seq(0.013, 0.05, length.out=10), seq(0.06, 0.368, length.out=10), 1), 
	browse=TRUE, verbose=TRUE
) {
	data(europawahl19) # tab.votes, tab.base, tab.gender.votes, tab.edu.votes
	
	# sets
	parties <- set(rownames(tab.votes))
	gender <- set("female", "male")
	edu <- set("Hochschule", "Abitur", "mittlere Reife", "Hauptschule")

	# decision variables
	prob <- variab(vote=parties, mf=gender, edu=edu)
	
	# set up mep
	mep <- mep_make(
		nvar=length(prob), 
		ncons=length(parties) + (length(gender)*length(edu)) + 1, 
		control=list(method="LP", storage="dense", breaks=breaks)
	)
	if(verbose) message("mep initialised.")
		
	# vote marginal distributions
	j <- 1
	for (x in parties) {
		mep <- mep_set_constraint(mep, j=j, xt=1, indices=index(prob, vote=x), rhs=tab.votes[x, "share"])
		j <- j + 1
	}
	if(verbose) message("vote marginal constraints added.")
	
	# edu & gender distribution marginal distributions
	for (x in gender)
		for (y in edu) {
			rhs <- tab.base[which(tab.base[,"gender"]==x & tab.base[,"education"]==y),"share"]
			mep <- mep_set_constraint(mep, j=j, xt=1, indices=index(prob, mf=x, edu=y), rhs=rhs)
			j <- j + 1
		}
	if(verbose) message("gender and education marginals added.")
		
	# must sum up to 1
	mep <- mep_set_constraint(mep, j=j, xt=rep(1, length(prob)), indices=1:length(prob), rhs=1)
	if(verbose) message("mep set up.")
	
	# transform to LP
	n <- length(mep$control$breaks)
	entropy <- approx_entropy(breaks=breaks)
	lprec <- make.lp(nrow=mep$ncons+mep$nvar*(n-1)+mep$nvar, ncol=mep$nvar*(n-1)+1)
	lp.control(lprec, sense="max")
	# browser()
	set.objfn(lprec, obj=c(rep(entropy$slopes, times=mep$nvar),10)) # objective function
	if(verbose) message("lp objective set.")
	
	set.constr.type(lprec, types=c(rep("=", times=mep$ncons), rep("<=", times=mep$nvar*(n-1)), rep(">=", times=mep$nvar)))
	set.rhs(lprec, b=c(mep$bvec, rep(diff(entropy$breaks), times=mep$nvar), rep(0, times=mep$nvar)))
	
	for (j in seq(mep$ncons)) set.row(lprec, row=j, xt=c(rep(mep$Amat[,j], each=(n-1)),0))
	if(verbose) message("mep constraints transfered to lp.")
	
	for (j in seq(mep$nvar*(n-1))) set.row(lprec, row=mep$ncons+j, xt=1, indices=j)
	if(verbose) message("segment boundaries contraints added to lp.")
	
	for (j in seq(mep$nvar)) {
		set.row(lprec, row=mep$ncons+mep$nvar*(n-1)+j, xt=c(rep(1, n-1), -1), indices=c(seq((j-1)*(n-1)+1, length.out=n-1), mep$nvar*(n-1)+1)) # min p_i
	}
	if(verbose) message("lp set up.")
	
	# solve
	status <- solve(lprec)
	if(status!=0) stop(paste("no solution found, error code ", status)) ### no solution? halt
	solution <- head(get.variables(lprec), -1)
	solution <- sapply(slice(solution, n-1), FUN=sum)
	if(verbose) message("lp solved, entropy=", sum(-log(solution)*solution))

	# prepare output
	ans <- vec2df(prob, solution)
	if(verbose) message("maxent distribution calculated")
	if(browse) browser()
	return(ans)
}
