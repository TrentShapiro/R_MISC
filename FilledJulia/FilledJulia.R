########
# Filled Julia
########

xmin = -2
xmax = 2
ymin = -1
ymax = 1

zreal = -0.8
zimaginary = 0.156

interval = 0.013


julia.frame <- data.frame(Real = rep(0,length(seq(xmin,xmax,interval))*length(seq(ymin,ymax,interval))), 
                               Imaginary = rep(0,length(seq(xmin,xmax,interval))*length(seq(ymin,ymax,interval))), 
                               InOut = 0 , 
                               Iterations = 0)

iter = 0

for ( x in seq(xmin,xmax,interval) ){
  for ( y in seq(ymin,ymax,interval) ){
    iter = iter+1
    z = complex (real = zreal , imaginary = zimaginary)
    julia.frame$Real[iter] <- x
    julia.frame$Imaginary[iter] <- y
    calc <- iterfunction(complex(real=x,imaginary=y), z , 0)
    julia.frame$InOut[iter] <- Re(calc[3])
    julia.frame$Iterations[iter] <- Re(calc[2])
    #julia.frame$Value[iter] <- calc[1]
    #print(iter)
  }
}
colorlookup <- gray.colors(length(unique(julia.frame$Iterations))+1)
julia.frame$Colors <- colorlookup[julia.frame$Iterations[julia.frame$Iterations>0]]

plot(julia.frame$Real,julia.frame$Imaginary, col = julia.frame$InOut)
plot(julia.frame$Real,julia.frame$Imaginary, col = julia.frame$Iterations)
plot(julia.frame$Real,julia.frame$Imaginary, col = julia.frame$Colors, pch=20)# , pch=16 , cex = 0.65)



########
#Only Inside - Increase Precision
########

julia.frame.inside <- julia.frame[julia.frame$InOut == 1,]

plot(julia.frame.inside$Real,julia.frame.inside$Imaginary, col = julia.frame.inside$Colors, pch=20)



iterfunction <- function (x, c, count, maxiter = 50){
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


