g = function (x) {
  x^2/sqrt(2*pi)*exp(-x^2/2)*(x > 1)
}
scaleRayleigh = 1.5
mean = 1.5
n = 1000
f1 = function (x) {
  drayleigh(x, scale = scaleRayleigh)*(x > 1)
}
f2 = function (x) {
  dnorm(x, mean = mean)*(x > 1)
}
rf1 = function () {
  rrayleigh(n, scale = scaleRayleigh)
}
rf2 = function () {
  rnorm(n, mean = mean)
}
is.rayleigh = function () {
  x = rf1()
  return(mean(g(x)/f1(x), na.rm = TRUE))
}

is.norm = function () {
  x = rf2()
  return(mean(g(x)/f2(x), na.rm = TRUE))
}

theta1 = is.rayleigh()
theta1
theta2 = is.norm()
theta2
exact = 0.400626
exact
