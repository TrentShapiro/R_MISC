arrrrr <- seq(0,5,0.01)

dev.off()
dev.on()
plot(0,0,xlim = c(0,5), ylim=c(0,1.1))
for ( x in seq (1.1,2,0.1)){
  for ( y in seq(1.1,1.2,0.1)){
    arrrrr.2 <- dweibull(arrrrr, x, y, log = FALSE)
    lines(arrrrr,arrrrr.2)
  }
}


