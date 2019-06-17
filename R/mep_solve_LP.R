mep_solve_LP <- function(mep, verbose=TRUE) {
  # transform to LP
  n <- length(mep$control$breaks)
  entropy <- approx_entropy(breaks=breaks)

  lprec <- make.lp(nrow=mep$ncons+mep$nvar*(n-1)+mep$nvar, ncol=mep$nvar*(n-1)+1)
  lp.control(lprec, sense="max")
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
  mep$prob <- head(get.variables(lprec), -1)
  mep$prob <- sapply(slice(mep$prob, n-1), FUN=sum)
  if(verbose) message("lp solved, entropy=", sum(-log(mep$prob)*mep$prob))
  return(mep)
}
