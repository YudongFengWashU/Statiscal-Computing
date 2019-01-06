set.seed(12345)
frechet_sample <- rfrechet(100,scale = 2,shape = 3)


loglikelihood <- function(alpha_s,x){
  alpha = alpha_s[1]
  s = alpha_s[2]
  n = length(x)
  return(
    n*log(abs(alpha))+(-1-alpha)*(sum(log(x)))-n*log(abs(s))-(-n-n*alpha)*log(abs(s)) + (sum((-x)^(-alpha)))/(s^(-alpha))
  )
}

grad_loglikelihood <- function(alpha_s,x){
  alpha = alpha_s[1]
  s = alpha_s[2]
  n = length(x)
  return(
    matrix(
      c( n/alpha - sum(log(x)) + n*log(s) - sum( ((s/x)^alpha) * log(s/x)  ) , 
         alpha*( n - sum( (s/x)^alpha  )   )/s
      ),2,1
      
    )
  )
}

quasiNewton <- function(init_par, grad, max_iter,data){
  par_old <- init_par
  iter_count <- 1
  h_old = diag(1,2,2)
  m = h_old
  rel_tol <- 2*tol

  
  while((rel_tol > tol) & (iter_count < max_iter) ){
   
    par_new <- par_old - h_old%*%grad(par_old,data)
    z = par_new - par_old
    y = grad(par_new,data) - grad(par_old,data)
    v = y - m%*%z
    c = 1/as.numeric(t(v)%*%z)
    h_new = h_old + ((z-h_old%*%y)%*%t(z-h_old%*%y))/as.numeric(t(z-h_old%*%y)%*%y)
    par_old <- par_new
    rel_tol <- max(abs(par_new - par_old)/par_old)
    iter_count <- iter_count + 1
    h_old <- h_new
    m = m + c*v%*%t(v)
    

  }
  return(par_new)
}
initalPar = c(1,1)
qNewpar=quasiNewton(initalPar,grad_loglikelihood,0.01,50,frechet_sample)
