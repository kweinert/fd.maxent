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
		ncons=length(parties) + (length(gender)*length(edu)) + nrow(tab.gender.votes) + 3*nrow(tab.edu.votes) + 1, 
		varinfo=prob,
		control=list(method="LP")
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
	
	# known survey results on gender
	voters <- setdiff(parties, "Nichtwähler")
	for (x in rownames(tab.gender.votes)) {
		prct <- tab.gender.votes[x, "male"] 
		mep <- mep_set_constraint(mep, j=j, 
			xt=c(rep(-1+prct, times=length(edu)), rep(prct, times=(length(voters)-1)*length(edu))), 
			indices=c(index(prob, vote=x, mf="male"), index(prob, vote=setdiff(voters,x), mf="male")), 
			rhs=0
		)
		j <- j + 1
	}
	if(verbose) message("gender vote survey results added.")
	
	# known survey results on education
	voters <- setdiff(parties, "Nichtwähler")
	for (x in rownames(tab.edu.votes)) 
		for (y in c("Hochschule", "Abitur", "Hauptschule")) { # leave one out "mittlere Reife"
			prct <- tab.edu.votes[x, y] 
			# browser()
			mep <- mep_set_constraint(mep, j=j, 
				xt=c(rep(-1+prct, times=length(gender)), rep(prct, times=(length(voters)-1)*length(gender))), 
				indices=c(index(prob, vote=x, edu=y), index(prob, vote=setdiff(voters,x), edu=y)), 
				rhs=0
			)
			j <- j + 1
	}
	if(verbose) message("education vote survey results added.")
		
	# must sum up to 1
	mep <- mep_set_constraint(mep, j=j, xt=rep(1, length(prob)), indices=1:length(prob), rhs=1)
	if(verbose) message("mep set up.")
	
	mep <- mep_solve(mep)
	solution <- mep_getvars(mep)
	mep_dispose(mep)
	View(solution)
}
