n = 10000 
#simple MC
u = runif(n)
eu = exp(u)
mean1 = mean(eu)
s = 0
for (i in 1:(n)){
  s = s + (eu[i] - mean1)^2
}
var1 = s/(n)
#antithetic
u1 = runif(n)
u2 = 1 - u1
eu1 = exp(u1)
eu2 = exp(u2)
eu3=(eu1+eu2)/2
mean2 = mean(eu3)
ss = 0
for (i in 1:n){
  ss = ss + (eu3[i] - mean2)^2
}
var2 = ss/(n)
1 - var2/var1
