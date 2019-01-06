my.bisect <- function(fun,interval,tol){
  a = interval[1]
  b = interval[2]
  while (b-a>tol){
    if (fun(a)*fun((a+b)/2) <= 0){
      b = (a+b)/2
    }else{
      a = (a+b)/2
    }
  }
  a
}

my.bisect.hm <- function(fun,interval,tol){
  a = interval[1]
  b = interval[2]
  k = 0
  while (b-a>tol){
    k = k + 1
    if (fun(a)*fun((a+b)/2) <= 0){
      b = (a+b)/2
    }else{
      a = (a+b)/2
    }
  }
  k
}


g <- function(x){
  log(x)/(1+x)
}

dg <- function(x){
  (1+x^(-1)-log(x))/((1+x)^2)
}
my.bisect(dg,c(1,5),0.001)
#cur <- function(a,k){
#  pt((k*a^2/(k+1-a^2))^0.5) - pt(((k-1)*a^2/(k-a^2))^0.5) 
#}
uniroot(dg,c(1,5),tol = 0.001)

k_seq = c(0)*25
tol=0.0001
for (k in 4:25){
  c = 0
  d = k^0.5
  while (d-c>tol){
    mid = (c+d)/2
    fun.c = pt((k*c^2/(k+1-c^2))^0.5,k) - pt(((k-1)*c^2/(k-c^2))^0.5,k-1)
    fun.mid = pt((k*mid^2/(k+1-mid^2))^0.5,k) - pt(((k-1)*mid^2/(k-mid^2))^0.5,k-1)
    if (fun.c*fun.mid < 0){
      d = (c+d)/2
    }else{
      c = (c+d)/2
    }
  }
  k_seq[k]=d
}
k_seq


k = 100
c = 0
d = k^0.5
while (d-c>tol){
  mid = (c+d)/2
  fun.c = pt((k*c^2/(k+1-c^2))^0.5,k) - pt(((k-1)*c^2/(k-c^2))^0.5,k-1)
  fun.mid = pt((k*mid^2/(k+1-mid^2))^0.5,k) - pt(((k-1)*mid^2/(k-mid^2))^0.5,k-1)
  if (fun.c*fun.mid < 0){
    d = (c+d)/2
  }else{
    c = (c+d)/2
  }
}
k_seq[1]=d

k = 500
c = 0.0001
d = 1.2*log(k)
while (d-c>tol){
  mid = (c+d)/2
  fun.c = pt((k*c^2/(k+1-c^2))^0.5,k) - pt(((k-1)*c^2/(k-c^2))^0.5,k-1)
  fun.mid = pt((k*mid^2/(k+1-mid^2))^0.5,k) - pt(((k-1)*mid^2/(k-mid^2))^0.5,k-1)
  if (fun.c*fun.mid < 0){
    d = (c+d)/2
  }else{
    c = (c+d)/2
  }
}
k_seq[2]=d

k = 1000
c = 0.0001
d = 1.2*log(k)
while (d-c>tol){
  mid = (c+d)/2
  fun.c = pt((k*c^2/(k+1-c^2))^0.5,k) - pt(((k-1)*c^2/(k-c^2))^0.5,k-1)
  fun.mid = pt((k*mid^2/(k+1-mid^2))^0.5,k) - pt(((k-1)*mid^2/(k-mid^2))^0.5,k-1)
  if (fun.c*fun.mid < 0){
    d = (c+d)/2
  }else{
    c = (c+d)/2
  }
}
k_seq[3]=d
k_seq
