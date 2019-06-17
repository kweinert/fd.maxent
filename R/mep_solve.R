# internal
# solves for one constraint
mep_solve_one <- function(mep, i, only_lambda=TRUE) {
	ans <- rep(NA, mep$nvar)
	if(mep$control$storage[1]=="sqlite") {
		Acol <- rep(0, mep$nvar)
		dat <- dbGetQuery(mep$Amat, paste(
			"select i,x from Amat",
			"where j=",i
		))
		Acol[dat[,"i"]] <- dat[,"x"]
	} else Acol <- mep$Amat[,i]
	
	optfun <- function(lambda) {
		y <- exp(Acol * lambda)
		Z <- sum(y)
		ans <- sum(Acol*as.numeric(y))/Z - mep$bvec[i]
		if(is.finite(ans)) return(ans) else return(sign(ans)*.Machine$double.xmax)
	}
	sol <- uniroot(optfun, interval=c(-708, 709/max(Acol)), tol=mep$control$tol) # exp(710)==Inf
	if(only_lambda) {
		return(sol$root)
	} else {
		mep$support <- as.numeric(Acol!=0)
		mep$prob <- exp(Acol * sol$root)
		mep$prob  <- mep$prob / sum(mep$prob)
		mep$lambda <- sol$root
		mep$status <- 0
		return(mep)
	}
}

mep_solve <- function(mep, verbose=TRUE) {
	if(mep$ncons==1) {
		if(verbose) message("using uniroot for one-constraint problem")
		return(mep_solve_one(mep, 1, only_lambda=FALSE))
	} 
	if(mep$control$method[1]=="BB") {
		if(mep$control$storage[1]=="sqlite") {
			if (dbExistsTable(mep$Amat, "lambda")) {
				message("found precomputed lambda, starting from there.")
				init <- dbReadTable(mep$Amat, "lambda")
				init <- init[order(init[,"j"]),]
				init <- init[,"x"]
			} else {
				init <- if(mep$control$init[1]=="runif") runif(mep$ncons) else mep$lambda
			}
			iidx <- seq.int(mep$nvar)
			yzero <- rep(0, mep$nvar)
			optfun <- function(lambda) {
				dbWriteTable(mep$Amat, "lambda", data.frame(j=seq.int(mep$ncons), x=lambda), overwrite=TRUE)
				ydat <- dbGetQuery(mep$Amat, paste(
					"select Amat.i, sum(Amat.x*lambda.x) from Amat",
					"join lambda on Amat.j=lambda.j",
					"group by Amat.i"
				))
				y <- yzero
				y[ydat[,1]] <- ydat[,2]
				y <- exp(y)
				Z <- sum(y)
				dbWriteTable(mep$Amat, "y", data.frame(i=iidx, y=y), overwrite=TRUE)
				bdat <- dbGetQuery(mep$Amat, paste(
					"select Amat.j, sum(Amat.x*y.y) from Amat",
					"join y on Amat.i=y.i",
					"group by Amat.j"
				))
				bhat <- rep(0, mep$ncons)
				bhat[bdat[,1]] <- bdat[,2]
				# browser()
				bhat / Z - mep$bvec
			} 
			# browser()
			optres <- BBsolve(par=init, fn=optfun, quiet=FALSE, control=list(tol=mep$control$tol, maxit=mep$control$maxit, trace=verbose, triter=1))
			mep$status <- optres$convergence
			if(mep$status==0) {
				dbWriteTable(mep$Amat, "lambda", data.frame(j=seq.int(mep$ncons), x=optres$par), overwrite=TRUE)
				ydat <- dbGetQuery(mep$Amat, paste(
					"select Amat.i, sum(Amat.x*lambda.x) from Amat",
					"join lambda on Amat.j=lambda.j",
					"group by Amat.i"
				))
				y <- yzero
				y[ydat[,1]] <- ydat[,2]
				mep$prob <- exp(y)
				mep$prob  <- mep$prob / sum(mep$prob)
				mep$lambda <- optres$par
			}
			
		} else { # dense / sparse	
			init <- if(mep$control$init[1]=="runif") runif(mep$ncons) else mep$lambda
			optfun <- function(lambda) {
				y <- exp(mep$Amat %*% lambda)
				Z <- sum(y)
				# browser()
				colSums(mep$Amat*as.numeric(y))/Z - mep$bvec
			} 
			optres <- BBsolve(par=init, fn=optfun, quiet=FALSE, control=list(tol=mep$control$tol, trace=verbose, triter=1))
			mep$status <- optres$convergence
			if(mep$status==0) {
				mep$prob <- exp(mep$Amat %*% optres$par) # dense / sparse
				mep$prob  <- mep$prob / sum(mep$prob)
				mep$lambda <- optres$par
			}
		}
		names(mep$prob) <- mep$varnames
		return(mep)
	} else stop("niy")
}
