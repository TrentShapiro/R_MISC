### Equation
## New Population = Growth Factor * Previous Population * ( 1 - Previous Population)


## Growth Rate 0-3
x = 100
counter = 1
precision = 0.001
startingPopulation = 0.5

GrowthRates <- seq(0,3,0.001);

output <- vector(mode = "double" , length = length(GrowthRates))
iterations <- vector(mode = "double" , length = length(GrowthRates))

for ( i in 1:length(GrowthRates)){
  thisGrowthRate = GrowthRates[i]
  counter = 1;
  x = 100;
  iter <- 0;

  
  while ( x > precision){
    if ( counter == 1 ){
    prevIter <- thisGrowthRate * startingPopulation * ( 1 - startingPopulation)
    counter <- 2
    } else {
      thisIter <- thisGrowthRate * prevIter * ( 1 - prevIter)
      x <- abs ( thisIter - prevIter)
      prevIter <- thisIter
      iter <- iter + 1
    }
  }
  
  output[i] <- thisIter  
  iterations[i] <- iter
}




plot(GrowthRates, output)
plot(GrowthRates[1:2900], iterations[1:2900])
