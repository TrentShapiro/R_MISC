library("e1071")

##playgolf
wd <- getwd()
playgolf <- read.csv(paste0(wd,"/PlayGolf.csv"))
playgolf$Windy<- as.factor(playgolf$Windy)

bayesmodel.golf <- naiveBayes(Decision ~ . , data = playgolf)
bayesmodel.golf
table(predict(bayesmodel.golf, playgolf[,-5]),playgolf[,5])


##iris
bayesmodel.iris <- naiveBayes(Species ~ . , data = iris)
bayesmodel.iris
table(predict(bayesmodel.iris , iris[,-5]), iris[,5])



