set.seed(1)
x<-seq(0.001,0.999,length.out=10000)
y<-dbeta(x,3,5)
z<-y/x
z1 <- max(z)
z<-y/(x-1)
z2 <- min(z)
x0 <- z2/(z2-z1)
y0 <- z1*x0
# The remaining vertex
# > x0
# [1] 0.2967033
# > y0
# [1] 2.552123
M = y0/2
h <- function(x) z1*x*(x<x0)+ z2*(x-1)*(x0<x)
G <- function(x) h(x)/M4

#5(c)
eff = 1/M
eff

plot(x0,y0,main='probability density function',xlim=c(0,1),ylim=c(0,3),ylab='PDF')
lines(x,dbeta(x,3,5),col='red')
lines(x[x<x0],z1*x[x<x0],col='blue')
lines(x[x>x0],z2*(x[x>x0]-1),col='blue')

#5(b)
# Use the inverse cdf method to sample from triangular distribution
# The pdf is g(x)= z1*x/M, 0<x<x0
#                  z2*(x-1)/M,x0<x<1
# The cdf is G(x)= (1/M)*(z1/2)*x^2, 0<x<x0
#                  (1/M)*((z2/2)*x^2 - z2*x+c), x0<x<1
#                     where c = (z1/2)*x0^2 - (z2/2)*x0^2 +z2*x0

# The inverse cdf is G_inv(u) = sqrt(2*u*M/z1), 0<u<(z1/2)*x0^2
#                             1 - sqrt((2/z2)*(u*M-c)+1),(z1/2)*x0^2<u<1


m <- 10000
u <- runif(m, 0, 1)
invcdf.func <- function(u) {
  if (u >= 0 && u <(z1/2)*x0^2)
    return(sqrt(2*u*M/z1))
  else if (u >=(z1/2)*x0^2 && u <= 1)
    c <- (z1/2)*x0^2 - (z2/2)*x0^2 +z2*x0
  
  
  return( 1 - sqrt((2/z2)*(u*M-c)+1))
}

r <- function(x) dbeta(x,3,5)/h(x)
nsamp <- 0
count <- 0
X <- rep(NA,m)

while(nsamp<m) {
  count <- count +1
  x <- invcdf.func(runif(1))
  if (runif(1)<=r(x)){
    nsamp <- nsamp + 1
    X[nsamp] <- x
  }
}
rate <- m/count


par(new=TRUE)
plot(density(X),main='probability density function',xlim=c(0,1),ylim=c(0,3),xlab='',ylab='PDF')

