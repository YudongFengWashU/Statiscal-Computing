library(VGAM)
g = function (x) {
  x^2/sqrt(2*pi)*exp(-x^2/2)
}
x = seq(0,6,0.1)
ys.g = g(x)
ys.rayleigh = drayleigh(x, scale = 1.5)
ys.norm = dnorm(x, mean = 1.5)
lim = max(c(ys.g, ys.rayleigh, ys.norm))
plot(x, ys.g, type = "l", ylim = c(0, lim))
lines(x, ys.rayleigh, col="red", ylim = c(0, lim))
lines(x, ys.norm, col="blue", ylim = c(0, lim))
# f1(x) = drayleigh(x, scale = 1.5)
# f2(x) = dnorm(x, mean = 1.5)
# f2 is closer to the function g, thus, f2 is better.
