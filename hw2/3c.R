
loglikelihood <- function(alpha_s){
  x = rfrechet(100,scale = 2,shape = 3)
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

optim(par = c(1,1),fn = loglikelihood,method = "BFGS")
