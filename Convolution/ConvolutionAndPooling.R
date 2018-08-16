## Convolution ##--------------------------------------------------------

x <- matrix(nrow=100,ncol = 100)



for(i in 1:100){
  for (j in 1:100){
    x[i,j] = round ( runif (1) , digits = 0 );
  }
}

mask <- list ( 1,0,1,
               0,0,0,
               1,0,1 )

y <- convolute ( x , mask)
image(t(apply(y,1,rev)))

y2 <- convolute ( y , mask)
image(t(apply(y2,1,rev)))

y3 <- convolute ( y2 , mask)
image(t(apply(y3,1,rev)))

y4 <- convolute ( y3 , mask)
image(t(apply(y4,1,rev)))


mask2 <- list ( 0,0,0,1,1,
                0,0,1,1,1,
                0,1,1,1,0,
                1,1,1,0,0,
                1,1,0,0,0)

y <- convolute ( x , mask2)
image(t(apply(y,1,rev)))

y2 <- convolute ( y , mask2)
image(t(apply(y2,1,rev)))

y3 <- convolute ( y2 , mask2)
image(t(apply(y3,1,rev)))

y4 <- convolute ( y3 , mask2)
image(t(apply(y4,1,rev)))

mask3 <- list ( 1,1,0,0,0,0,0,
                1,1,1,0,0,0,0,
                0,1,1,1,0,0,0,
                0,0,1,1,1,0,0,
                0,0,0,1,1,1,0,
                0,0,0,0,1,1,1,
                0,0,0,0,0,1,1)

y <- convolute ( x , mask3)
image(t(apply(y,1,rev)))

y2 <- convolute ( y , mask3)
image(t(apply(y2,1,rev)))

y3 <- convolute ( y2 , mask3)
image(t(apply(y3,1,rev)))

y4 <- convolute ( y3 , mask3)
image(t(apply(y4,1,rev)))

convolute <- function (matrix , convmask){

  stopifnot( sqrt(length(convmask))%%2 != 0)
  
  convsize = sqrt(length(convmask))
  convcenter = ceiling ( convsize / 2)
  
  matrixrows <- nrow(matrix)
  matrixcols <- ncol(matrix)
  
  xstart <- convcenter
  ystart <- convcenter
  
  
  matrixout <- matrix(0 ,
                      nrow = (nrow(matrix) - 2 * (xstart - 1)) ,
                      ncol = (nrow(matrix) - 2 * (ystart - 1)) )
  
  
  for (i in xstart:(nrow(matrix) - (xstart - 1) )){   #x1
    for (j in ystart:(ncol(matrix) - (ystart - 1) )){ #y1
      for ( k in 1:convsize){                         #x2
        for (l in 1:convsize){                        #y2
          thisconvindex <- (convsize * (k-1)) + l
          thisconvvalue <- convmask[[thisconvindex]]
          thismatrixindexx <- (i - xstart) + k
          thismatrixindexy <- (j - ystart) + l
          thismatrixvalue <- matrix[thismatrixindexx , thismatrixindexy]
          addedvalue = thisconvvalue * thismatrixvalue
          matrixout[i - ( xstart - 1 ),j - (ystart - 1)] <- matrixout[i - ( xstart - 1 ),j - (ystart - 1)] + addedvalue
        }
      }
    }
  }
  
  return(matrixout)
}

pool <- function (matrix , convmask){
  
  stopifnot( sqrt(length(convmask))%%2 != 0)
  
  convsize = sqrt(length(convmask))
  convcenter = ceiling ( convsize / 2)
  
  matrixrows <- nrow(matrix)
  matrixcols <- ncol(matrix)
  
  xstart <- convcenter
  ystart <- convcenter
  
  
  matrixout <- matrix(0 ,
                      nrow = (nrow(matrix) - 2 * (xstart - 1)) ,
                      ncol = (nrow(matrix) - 2 * (ystart - 1)) )
  
  
  for (i in xstart:(nrow(matrix) - (xstart - 1) )){   #x1
    for (j in ystart:(ncol(matrix) - (ystart - 1) )){ #y1
      for ( k in 1:convsize){                         #x2
        for (l in 1:convsize){                        #y2
          thismatrixindexx <- (i - xstart) + k
          thismatrixindexy <- (j - ystart) + l
          thismatrixvalue <- matrix[thismatrixindexx , thismatrixindexy]
          if ( matrixout[i - ( xstart - 1 ),j - (ystart - 1)] < thismatrixvalue ){
            matrixout[i - ( xstart - 1 ),j - (ystart - 1)] <- thismatrixvalue
          }
        }
      }
    }
  }
  
  return(matrixout)
}


############# IMAGEIFY
wd <- getwd()

number <- read.csv (paste0(wd,'/binaryimage_file.txt') , header = FALSE , sep = ",")

imageify <- function (x,filtersize) {
  
  matrixout <- matrix(0 , nrow = 28 , ncol = 28)
  
  for ( i in 1:28){
    for ( j in 1:28){
      thisindex <- ( 28 * (i-1) ) + j
      thispixel <- x$V2[thisindex]
      if (thispixel > (255-filtersize)){
        thispixel <- 1
      } else {
        thispixel <- 0
      }
      matrixout[i,j] <- thispixel
    }
  }
  return (matrixout)
}

#numbermat <- imageify(number, 50)
numbermat <- matrix(number$V2 , ncol = 28 , nrow = 28)
image(numbermat)


## Reduce to 25x25
numbermat.reduce <- numbermat[2:26 , 4:28]
image(numbermat.reduce)

mask3 <- list ( 0,0,0,0,0,
                0,0,0,0,0,
                0,0,0,0,0,
                0,0,0,0,0,
                0,0,0,0,0)

mask2 <- list ( 0,0,1,
                0,1,0,
                1,0,0)

mask2.matrix <- matrix(data = )
image(t(apply(mask2mat,1,rev)))

y <- convolute ( numbermat.reduce , mask2)
y <- y / max(y)
image(y)

y.reduced <- pool ( y , mask3)
y.reduced <- y.reduced / max(y.reduced)
image(y.reduced)

y2 <- convolute ( y.reduced , mask2)
y2 <- y2/max (y2)
image(y2)

y2.reduced <- pool ( y2 , mask3 )
image(y2.reduced)

y3 <- convolute ( y2.reduced , mask2)
image(y3)

y4 <- convolute ( y3 , mask2)
image(y4)

y5 <- convolute ( y4 , mask2)
image(y5)

