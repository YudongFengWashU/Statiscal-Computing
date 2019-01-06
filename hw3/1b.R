simulated_annealing=function(x,y,p,n){
  for(j in 1:n){
    change = sample(1:15,size = 1)
    b=p
    b[change] = 1 - p[change]
    
    if(rxy(x,y)-rxy(x,y)>0){
      p=b
    }else{
      prob = min(1,exp((r(b)-r(p))/((n-j)/n)))
      if(rbinom(1,1,prob) == 1){
        p = b
      }else{p = p}
    }
  }
  return(list(rxy(p),p))
}
