library(RTriangle)
wd <- getwd()

canvas = 1000
tol = 0.01 * canvas
scale = 250
ncolor = 1000
maxi = 120

cscale <- colorRampPalette(c("#111111","#FFFFFF"),interpolate="spline")
cscale <- cscale(ncolor)

i = 1;

vert <- data.frame(x = c(0,-400,-400,400,400) , y = c(0,-400,400,400,-400) )
while ( i < maxi){
  newx <- runif(1 , -canvas , canvas)
  newy <- runif(1 , -canvas , canvas)
  if ( length(vert$x[vert$x < newx + tol & vert$x > newx - tol]) == 0 ){
    if ( length(vert$y[vert$y < newy + tol & vert$y > newy - tol]) == 0 ){
      vert <- rbind (vert,c(newx,newy))
      i = i + 1;
      print(paste0("found point",i))
    }
  }
}

p <- pslg(P=vert)
tp <- triangulate(p)
high <- canvas - scale
low <- -canvas + scale
plot(tp , xlim = c(low , high) , ylim = c(low , high) , pch='.')
#tiff(paste0(wd,"/Output/Output.tiff"), width = 16, height = 9, units = 'in', res = 1000)
plot(tp$P[1,], xlim = c(low , high) , ylim = c(low , high) ,bty = "n" ,
     pch='.', axes = F , ann = F , xlab = NA , ylab = NA, xaxs="i",yaxs="i")
for (i in 1:nrow(tp$T)){
  x <- tp$P[tp$T[i,],1]
  y <- tp$P[tp$T[i,],2]
  colval <- floor(runif(1,1,ncolor))
  polygon(x,y, col = cscale[colval] , border = NA )
}
#dev.off()

