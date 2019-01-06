set.seed(12345)
num_nodes=15
A=matrix(round(runif(num_nodes*num_nodes)),num_nodes,num_nodes)
diag(A)=0
A[lower.tri(A)]=A[upper.tri(A)]

rxy=function(x,y){
  l_x=length(x)
  l_y=length(y)
  d_c=2*choose(l_x,2)
  d_p=2*choose(l_y,2)
  d=d_c+d_p
  v_c=0
  v_p=0
  for (i in 1:l_x){
    for (j in 1:l_x){
      if (i!=j)
        v_c=v_c+1-A[x[i],x[j]]
    }
  }
  for (i in 1:l_y){
    for (j in 1:l_y){
      v_p=v_p+A[y[i],y[j]]
    }
  }
  sx=(d_c*d_p)^0.5/d
  sy=((d_c-v_c+v_p)*(d_p-v_p+v_c))^0.5/d
  sxy=(d_c-v_c)/d-(d_c/d)*(d_c-v_c+v_p)/d
  if(sxy==0){return(0)}
  else{return(sxy/(sx*sy))}
}


search=function(x,y,p){
  k=length(x)
  m=length(x)
  x1=x
  x2=x
  y1=y
  y2=y
  z=x
  w=y
  if(k>1){
    while (k>=2){
      X=matrix(0,k,k-1)
      Y=matrix(0,k,p+1-k)
      r=0*c(1:k)
      for (i in 1:k){
        X[i,]=x[-i]
        Y[i,]=c(y,x[i])
        r[i]=rxy(X[i,],Y[i,])
      }
      if (max(r)<=rxy(x,y)){
        break
      }else{
        x=X[which.max(r),]
        y=Y[which.max(r),]
        k=k-1
      }
    }
    x1=x
    y1=y
  }
  x=z
  y=w
  if(m<(p-1)){
    while (m<=(p-2)){
      X=matrix(0,p-m,m+1)
      Y=matrix(0,p-m,p-m-1)
      r=0*c(1:(p-m))
      for (i in 1:(p-m)){
        X[i,]=c(x,y[i])
        Y[i,]=y[-i]
        r[i]=rxy(X[i,],Y[i,])
      }
      if (max(r)<=rxy(x,y)){
        break
      }else{
        x=X[which.max(r),]
        y=Y[which.max(r),]
        m=m+1
      }
    }
    x2=x
    y2=y
  }
  if(rxy(x1,y1)>=rxy(x2,y2)){
    return(x1)
  }
  else{
    return(x2)
  }
  
}

