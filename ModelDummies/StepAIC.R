data(iris)

fit <- lm(iris$Species ~ ., data = iris)
fit
predicted <- predict(fit,iris)


install.packages("MASS")
library("MASS")
newfit <- stepAIC(fit,direction = "both")
class(a)


if(a != 'Regular'){
  modelerModel <- stepAIC(modelerModel, direction = a)
}

a <- 'Regular'


target <- 'MVCC'
data&target

x <- as.name('Sepal.Width')
iris$x

error <- iris[,colnames(iris)==x] - predicted

b <- cbind(iris,a)
View(a)
View(predicted)
a

(paste0(colnames(iris),collapse="+"))

View(combn(colnames(iris),2,FUN = function(x){paste0(x,collapse='*')}))


subset <- iris[,colnames(iris) != 'Partition']

a <- combn(colnames(subset),2, FUN = function(x){paste0(x,collapse='*')})
View(a)
newtarget <- paste0(a,collapse="+")

as.name(target)
as.name(a)
as.name(newtarget)
lm(as.formula(paste0(as.name(target),'~',as.name(newtarget))),data=iris)
target <- "Species"




modelerModel <- lm(as.formula(paste0(as.name(target),'~',as.name(target))), modelerData)


b <- strsplit(newtarget,"[+]")[[1]]
b 

c <- combn(b,2,FUN = function(x){paste0(x,collapse='*')})
