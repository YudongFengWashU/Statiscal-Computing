boxmmc <- function(mu,sigma,n,N){
  u <- runif(N)
  v <- runif(N)
  Y <- sqrt(-2 * log(u)) * cos(2 * pi * v)
  Z <- exp(mu+(sigma/sqrt(n))*Y)
  E <- sum(Z)/N
  E
}
