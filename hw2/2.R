set.seed(12345)
weibull_sample <- rweibull(50,5,7)
k = 2
contraction_mapping <- function(theta,x){
  return( ( sum(x^5)/(50*theta^(k)) )^(1/(5-k))) 
}
theta_last = 10
theta_next = 9.9
while(abs(theta_next - theta_last) > 0.01){
  theta_last <- theta_next
  theta_next <- contraction_mapping(theta_last,weibull_sample)
  
}
theta_next
(sum(weibull_sample^5)/50)^(1/5)

