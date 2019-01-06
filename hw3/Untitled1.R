x = c(1:14)
y = c(15)
l_x = length(x)
l_y = length(y)
d_x = 2*choose(l_x,2)
d_y = 2*choose(l_y,2)
d = d_x + d_y
v_x = 0
v_y = 0
for (i in 1:l_x){
  for (j in 1:l_x){
    v_x = v_x + 1 - A[x[i],x[j]]
  }
}
for (i in 1:l_y){
  for (j in 1:l_y){
    v_y = v_y + 1 - A[y[i],y[j]]
  }
}
sx = (d_x*d_y)^0.5/d
sy = ((d_x-v_x+v_y)*(d_y-v_y+v_x))^0.5/d
sxy_sq = (d_x-v_x)/d - (d_x/d)*(d_x-v_x+v_y)/d
rxy = sxy_sq/(sx*sy)

