kheko <- function() {
	profit_fun <- function(x) {
		if(x<20) 
			return(5*x)
		else if(x<40) 
			return(5*20+4*(x-20))
		else
		    return(5*20+4*20+3*(x-40))
	}
	profit_fun <- Vectorize(profit_fun)
	x <- seq(0,80, length.out=200)
	y <- profit_fun(x)
	plot(x,y, type="l")
	
	breaks <- c(0,20,40,60)
	slopes <- (profit_fun(tail(breaks,-1))-profit_fun(head(breaks,-1))) / diff(breaks)

	lprec <- make.lp(0, ncol=4)
	lp.control(lprec, sense="max")
	set.objfn(lprec, obj=c(4,slopes))
	add.constraint(lprec, xt=c(1,2,2,2), type="<=", rhs=115)
	add.constraint(lprec, xt=c(2,1,1,1), type="<=", rhs=80)
	add.constraint(lprec, xt=c(0,1,1,1), type="<=", rhs=60)
	for (i in 1:3) add.constraint(lprec, xt=1, indices=1+i, type="<=", rhs=breaks[i+1]-breaks[i])
	
	status <- solve(lprec)
	if(status!=0) stop(paste("no solution found, error code ", status)) ### no solution? halt
	solution <- get.variables(lprec)
	solution <- c(solution[1], sum(solution[2:4]))
	profit <- get.objective(lprec)
	
	stopifnot(isTRUE(all.equal(solution,c(15, 50))))
	stopifnot(isTRUE(all.equal(profit,270)))
	browser()
}