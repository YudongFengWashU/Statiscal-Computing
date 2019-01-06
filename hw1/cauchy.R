cauchycdf <- function(x,eta,theta){
  delta_x = seq(from=min(-abs(x),eta)-1000*theta,to=x,by=0.001)
  d = (((delta_x-eta)/theta)^2+1)*theta*pi
  0.001*sum(d^(-1))
}
