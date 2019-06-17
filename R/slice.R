slice<-function(x,n) {
    N <- length(x)
    lapply(seq(1,N,n), function(i) x[i:min(i+n-1,N)])
}