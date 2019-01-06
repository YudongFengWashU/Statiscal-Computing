set.seed(122)
data_ga <- rgamma(50,1,2)

gamma_loglikelihood <- function(k_theta, data){
  
  k <- k_theta[1]
  theta <- k_theta[2]
  n <- length(data)
  
  return( 
    
    -n*log(gamma(k)) - k*n*log(theta) + (k-1)*sum(log(data)) - (1/theta)*sum(data)
    
  )
}

gradient_gamma_loglikelihood <- function(k_theta, data){
  
  k <- k_theta[1]
  theta <- k_theta[2]
  n <- length(data)
  
  return( 
    
    matrix( c(  -n*digamma(k)-n*log(theta)+sum(log(data)) , 
                
                -k*n/theta + sum(data)/(theta^2)   ) , 2, 1 )
    
  )
}

hessian_gamma_loglikelihood <- function(k_theta, data){
  
  k <- k_theta[1]
  theta <- k_theta[2]
  n <- length(data)
  
  return( 
    
    matrix( c(  
      
      -n*trigamma(k), -n/theta,
      
      -n/theta,  k*n/(theta^2) - 2*sum(data)/(theta^3)
      
    ) , 2, 2 )
    
  )
}

inv_hessian_gamma_loglikelihood <- function(k_theta, data){
  
  H <- hessian_gamma_loglikelihood(k_theta, data)
  
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
  
  while( iter_count < max_iter){
    
    par_new <- par_old - inv_hess(par_old, data)%*%grad(par_old, data)
    rel_tol <- max(abs(par_new - par_old)/par_old)
    par_old <- par_new
    iter_count <- iter_count + 1
    iter_par[iter_count+1,] <- t(par_new)
    
  }
  
  return(list(solution = par_new, fun_solution = fun(t(par_new), data), final_tol = rel_tol, num_iters = iter_count, all_iters = iter_par[1:iter_count,]))
  
}
iniPar = c(1,1)

newtons_method(iniPar,gamma_loglikelihood,gradient_gamma_loglikelihood,inv_hessian_gamma_loglikelihood,0.1,30,data_ga)
