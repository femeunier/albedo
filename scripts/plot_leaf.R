library("rgl")




Xref = c(0,-1,1,0)
Yref = c(5,0,0,-2)
Zref = c(0,0,0,0)

cmbs_seg <- rbind(c(1,2),
              c(1,3),
              c(2,4),
              c(3,4))

cmbs <- rbind(c(1,2,4),
              c(1,3,4))

angles <- c(0,-50)*pi/180

clrs <- c("darkgreen","green")

for (ileaf in seq(N)){

  pos <- cbind(Xref,Yref,Zref)
  rot <- rbind(c(cos(angles[ileaf]),-sin(angles[ileaf]),0),c(sin(angles[ileaf]),cos(angles[ileaf]),0),c(0,0,1))
  # rot <- rbind(c(cos(angles[ileaf]),0,sin(angles[ileaf])),c(0,1,0),c(-sin(angles[ileaf]),0,cos(angles[ileaf])))
  rot <- rbind(c(1,0,0),c(0,cos(angles[ileaf]),-sin(angles[ileaf])),c(0,sin(angles[ileaf]),cos(angles[ileaf])))
  
  pos_prime = pos%*%rot
  
  Xprim = pos_prime[,1]
  Yprim = pos_prime[,2]
  Zprim = pos_prime[,3]

  # for(i in seq(1,nrow(cmbs_seg))){
  # 
  #   X = (Xprim[cmbs_seg[i,]] + delta_x[ileaf])
  #   Y = (Yprim[cmbs_seg[i,]] + delta_y[ileaf])
  #   Z = (Zprim[cmbs_seg[i,]] + delta_z[ileaf])
  #   
  #   segments3d(X,
  #              Y,
  #              Z,color=clrs[ileaf])
  # }
  
  for(i in seq(1,nrow(cmbs))){
    X = (Xprim[cmbs[i,]] + delta_x[ileaf])
    Y = (Yprim[cmbs[i,]] + delta_y[ileaf])
    Z = (Zprim[cmbs[i,]] + delta_z[ileaf])
    
    triangles3d(X,
                Y,
                Z,color=clrs[ileaf])
  }
}
# axes3d()