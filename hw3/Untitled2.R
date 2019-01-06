k = 14
x = c(1:14)
y = c(15)

 while (k>=1){
   X = matrix(0,k,k-1)
   Y = matrix(0,k,16-k)
   r = 0*c(1:k)
   for (i in 1:k){
     X[i,] <- x[-i]
     Y[i,] <- c(y,x[i])
     r[i]  <- rxy(X[i,],Y[i,])
   }
   if (max(r) <= rxy(x,y)){
     break
   }else{
     x = X[which.max(r),]
     y = Y[which.max(r),]
     k = k - 1
   }
 }
