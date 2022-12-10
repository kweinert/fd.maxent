#' Slice a vector into a list where each element has at most n elements
#'
#' @param x a vector
#' @param n integer, maximum length
#' @return a list
#' @export
slice<-function(x,n) {
    N <- length(x)
    lapply(seq(1,N,n), function(i) x[i:min(i+n-1,N)])
}