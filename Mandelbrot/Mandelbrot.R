###########
#Mandelbrot
###########

## Gloabl variables
xmin = -2
xmax = 1
ymin = -1.5
ymax = 1.5
interval = 0.01

## Function to iterate
iterfunction <- function (x, c, count, error=0.000001, maxiter = 100){
  thisiter <- x^2 + c
  count <- count + 1
  if (count >= maxiter){#|| sqrt( (Re(x) - Re(thisiter))^2 + (Im(x) - Im(thisiter))^2 ) <= error){
    output = c(thisiter, count, 1)
    return (output)
    break
  }else if ( abs(Re(x)) > 2 || is.infinite(Re(x)) || is.nan(Re(x)) || is.na(Re(x))){
    output = c(thisiter, count, 0)
    return (output)
    break
  }else{
    #output = c(thisiter, count, 0)
    #print(output)
    iterfunction(thisiter , c , count)
  }
}

## Output Frame
mandelbrot.frame <- data.frame(Real = rep(0,length(seq(xmin,xmax,interval))*length(seq(ymin,ymax,interval))), 
                               Imaginary = rep(0,length(seq(xmin,xmax,interval))*length(seq(ymin,ymax,interval))), 
                               InOut = 0 , 
                               Iterations = 0)

## Loop over a bunch of points
## Replace loop with function application to x,y column pair in a dataframe
iter = 0

for ( x in seq(xmin,xmax,interval) ){
  for ( y in seq(ymin,ymax,interval) ){
    iter = iter+1
    z = 0
    mandelbrot.frame$Real[iter] <- x
    mandelbrot.frame$Imaginary[iter] <- y
    calc <- iterfunction(z, complex(real=x,imaginary=y) , 0)
    mandelbrot.frame$InOut[iter] <- Re(calc[3])
    mandelbrot.frame$Iterations[iter] <- Re(calc[2])
    #mandelbrot.frame$Value[iter] <- calc[1]
  }
}
colorlookup <- gray.colors(length(unique(mandelbrot.frame$Iterations))+1)
mandelbrot.frame$Colors <- colorlookup[mandelbrot.frame$Iterations]

plot(mandelbrot.frame$Real,mandelbrot.frame$Imaginary, col = mandelbrot.frame$InOut)
plot(mandelbrot.frame$Real,mandelbrot.frame$Imaginary, col = mandelbrot.frame$Iterations)
plot(mandelbrot.frame$Real,mandelbrot.frame$Imaginary, col = mandelbrot.frame$Colors, pch=20)# , pch=16 , cex = 0.65)



