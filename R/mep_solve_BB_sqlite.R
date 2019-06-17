mep_solve_BB_sqlite <- function(mep, verbose=TRUE) {
  if (dbExistsTable(mep$Amat, "lambda")) {
    if(verbose) message("found precomputed lambda, starting from there.")
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
    bhat / Z - mep$bvec
  } 
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
  return(mep)
}