Dn <- function(theta,q,datax){
  n = length(datax)
  s = 0
  for (i in 1:n){
    s = s + abs(theta - datax[i]) - datax[i]
  }
  D = 0.05*(s/(2*n) + theta*(1-2*q)/2)
  return(D)
}

norwegian.fire <- read.delim("https://www.math.wustl.edu/~nasyring/475/norwegianfire.txt")
X.old <- norwegian.fire$X520[norwegian.fire$X72 == 89]/500
X <- norwegian.fire$X520[norwegian.fire$X72 == 90]/500

p.shape <- 2
p.scale <- as.numeric(quantile(X.old, 0.995)/2)


dino <- function(theta_p,X){
  p.shape <- 2
  p.scale <- as.numeric(quantile(X.old, 0.995)/2)
  n = length(X)
  exp(-n*Dn(theta_p,0.995,X))*theta_p^(p.shape - 1)*exp(-theta_p/p.scale)/(gamma(p.shape)*p.scale^p.shape)
}

a = 0
b = 199
m = 10000
x.seq <- seq(from = a, to = b, length.out = m) 
iterative_sum <- 0
for(i in 1:(m-1)){
  iterative_sum <- iterative_sum + dino(x.seq[i],X)+4*dino(0.5*(x.seq[i]+x.seq[i+ 1]),X)+dino(x.seq[i+1],X)
}
dino_simp = ((b-a)/(6*m))*iterative_sum

theta = seq(from = a, to = b, length.out = m)
n = length(X)
numerator = exp(-n*Dn(theta,0.995,X))*theta^(p.shape - 1)*exp(-theta/p.scale)/(gamma(p.shape)*p.scale^p.shape)
plot(theta,numerator/dino_simp)

theta[which.max(numerator/dino_simp)]
