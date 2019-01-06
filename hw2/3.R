set.seed(12345)
frechet_sample <- rfrechet(100,scale = 2,shape = 3)

loglikelihood <- function(alpha_s,x){
  alpha = alpha_s[1]
  s = alpha_s[2]
  n = length(x)
  return(
    n*log(alpha)+(-1-alpha)*(sum(log(x)))-n*log(s)-(-n-n*alpha)*log(s) + (sum((-x)^(-alpha)))/(s^(-alpha))
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


hessian_loglikelihood <- function(alpha_s,x){
  alpha = alpha_s[1]
  s = alpha_s[2]
  n = length(x)
  return(
    matrix(c(
      -n/alpha^2 - sum( (s/x)^alpha * (log(s/x))^2     ) ,
      n/s - sum((s/x)^alpha)/s -alpha*sum( (s/x)^alpha * log(s/x) )/s ,
      n/s - sum((s/x)^alpha)/s -alpha*sum( (s/x)^alpha * log(s/x) )/s ,
      n/s^2*(sum((s/x)^alpha) - n) - alpha^2/s^2 * sum((s/x)^alpha)
    ),2,2,byrow = TRUE
    )
  )
}


inv_hessian_loglikelihood <- function(alpha_s,x){

  H <- hessian_loglikelihood(alpha_s,x)
  
  return(
    
    (1/(H[1,1]*H[2,2]-H[1,2]*H[2,1]))*matrix(c(H[2,2], -H[2,1], -H[1,2], H[1,1]),2,2)
    
  )
}


newtons_method <- function(init_par, fun, grad, inv_hess, tol, max_iter, data){
  
  rel_tol <- 2*tol
  par_old <- init_par
  iter_count <- 0
  iter_par <- matrix(0,max_iter+1,length(init_par))
  iter_par[1,] <- t(init_par) 
  
  while( (rel_tol > tol) & (iter_count < max_iter) ){
    
    par_new <- par_old - inv_hess(par_old, data)%*%grad(par_old, data)
    rel_tol <- max(abs(par_new - par_old)/par_old)
    par_old <- par_new
    iter_count <- iter_count + 1
    iter_par[iter_count+1,] <- t(par_new)
    
  }
  
  return(list(solution = par_new, fun_solution = fun(t(par_new), data), final_tol = rel_tol, num_iters = iter_count, all_iters = iter_par[1:iter_count,]))
  
}

initalPar = c(1,1)
newtons_method(initalPar,loglikelihood,grad_loglikelihood,inv_hessian_loglikelihood,0.01,99,frechet_sample)
