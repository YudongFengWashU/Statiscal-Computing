#a
N = 10000
x = runif(N,0,pi/4)
y = runif(N,0,pi/4)
g <- function(x,y){
  x^2*y^2*sin(x+y)*log(x+y)*((pi/4)^2)
}
mean(g(x,y))

#b
f1 <- function(x,y){
  x^3*y^3
}
B1 = f1(x,y)
A1 = g(x,y)
c1 = -cov(A1,B1)/var(B1)
T1 = g(x,y)
T2 = T1 + c1*(f1(x,y) - ((pi/4)^6)/16)
c(mean(T1),mean(T2))
c(var(T1),var(T2))
(var(T1) - var(T2))/var(T1)

#c
f2 <- function(x,y){
  x^2*y^2
}
B2 = f2(x,y)
A2 = g(x,y)
c2 = -cov(A2,B2)/var(B2)
S1 = g(x,y)
S2 = S1 + c2*(f2(x,y) - ((pi/4)^4)/9)
c(mean(S1),mean(S2))
c(var(S1),var(S2))
(var(S1) - var(S2))/var(S1)

#d
A = g(x,y)
lm_c1 = -lm(A ~ B1 + B2)$coefficients[2]
lm_c2 = -lm(A ~ B1 + B2)$coefficients[3]
lm_T = g(x,y) + lm_c1*(f1(x,y) - ((pi/4)^6)/16) + lm_c2*(f2(x,y) - ((pi/4)^4)/9)
c(mean(T1),mean(T2),mean(lm_T))
c(var(T1),var(T2),var(lm_T))
(var(T2) - var(lm_T))/var(T2)
(var(T1) - var(lm_T))/var(T1)