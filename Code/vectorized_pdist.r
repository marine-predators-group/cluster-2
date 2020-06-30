vectorized_pdist <- function(A,B){
  # this function was found at: http://blog.felixriedel.com/2013/05/pairwise-distances-in-r/
  # and is a faster, vectorized implementation of pdist::pdist
  # see real pdist function for help
  
  an = apply(A, 1, function(rvec) crossprod(rvec,rvec))
  bn = apply(B, 1, function(rvec) crossprod(rvec,rvec))
  
  m = nrow(A)
  n = nrow(B)
  
  tmp = matrix(rep(an, n), nrow=m) 
  tmp = tmp +  matrix(rep(bn, m), nrow=m, byrow=TRUE)
  sqrt(tmp - 2 * tcrossprod(A,B))
}