require(jpeg)
wd <- getwd()
########
#ASCII Characters 
########
characters.available <- c("~","`","@","#","&","%","$",
                          "!","^","*","(",")",
                          "-","_","+","=","[","]",
                          "{","}","|","\\",":",";",
                          "\"","<",">",",",".","?","/"," ")

########
# Create imgs of ascii chars to compare
########

#Precision = number of rows & columns in square matrix
precision <- 3

### Create data.frame to hold values of look
characters.matrix <- data.frame ( characters = as.character(characters.available))
#characters.matrix$avg <- 0
for (i in 1:precision){
 for (j in 1:precision){
   string.value <- paste ( "characters.matrix$x" , i , "y" , j , "<- 0" ,sep = "")
   eval ( parse(text = string.value))
 }
}

 ### Save ascii characters as images
 for ( x in 1:length(characters.available)){
   this.char <- characters.available[x];
   X11(width = 2 , height = 2)
   plot.new()
  par(mar=c(0,0,0,0))
  plot.window(c(0,1),c(0,1))
  text( x = 0.08 , y = 0.5 , labels = paste("l " , characters.available[x]) , ann = FALSE , cex = 12)
  #plot( x = 0.5 , y = 0.5 , pch = "@" , axes = FALSE , ann = FALSE , cex = 12)

  savePlot(filename = paste0(wd,"/ASCII_Characters/img",as.character(x)) , type = "jpg")
  dev.off()
}

####
#Load ascii images and assign compare values
####
for ( i in 1:length(characters.available)){

  ### readJPEG
  this.char.image <- readJPEG(source = paste0(wd,"/ASCII_Characters/img",i,".jpg"))
  
  ### readJPEG separates RGB, just add them all across rows,columns
  this.char.image1 <- apply(this.char.image , c(1,2), sum)
  
  ### Apply reverse filter - readJPEG Loads white as 1, black as 0, need to reverse 
  this.char.image1 <- ifelse ( this.char.image1 > 0.75 , 0 , 1)
  
#  characters.matrix$avg[i] <- sum(this.char.image1)/(nrow(this.char.image1)*ncol(this.char.image1))
  
  ## Assign data.frame values
  for (x in 1:precision){
    for (y in 1:precision){
      increment.length.x <- floor(nrow(this.char.image1) / precision)
      increment.length.y <- floor(ncol(this.char.image1) / precision)

      starting.x <- ((increment.length.x * x) - increment.length.x) + 1
      starting.y <- ((increment.length.y * y) - increment.length.y) + 1

      ending.x <- starting.x + increment.length.x - 1
      ending.y <- starting.y + increment.length.y - 1

      sector.value <- sum(this.char.image1[starting.x:ending.x ,starting.y:ending.y])
      column.string <- paste0("x",x,"y",y)

      characters.matrix[i,column.string] <- sector.value
    }
  }
}

#### Normalize the comparison matrix
characters.matrix[,-1] <- round ( characters.matrix[,-1] / max(characters.matrix[,-1]) , 5 )
characters.matrix$characters <- as.character(characters.matrix$characters)


########
# Load image to convert
########
sourceimg <- "Sloth"
sourcefile <- paste0(wd,"/ASCII_Input/",sourceimg)
outputfile <- paste0(wd,"/ASCII_Output/",sourceimg)

base.image <- readJPEG(source=paste0(sourcefile , ".jpg") )

## Sum Channels and apply filter
base.image <- apply(base.image,c(1,2),sum)
base.image <- 2 - base.image
base.image <- ifelse(base.image < 1 , 0 , base.image)
base.image <- base.image/max(base.image)

## Set up variables and output matrix
mask.x.length <- precision^2
mask.y.length <- precision^2

output.rows <- floor(nrow(base.image) / mask.x.length)
output.cols <- floor(ncol(base.image) / mask.y.length)

output.matrix <- matrix ( "0" , nrow = output.rows , ncol = output.cols)

## For each square, find the closest ascii character
for ( i in 1:output.rows){
  for ( j in 1:output.cols){

    ## Subset main image
    main.starting.x <- (i * mask.x.length) - mask.x.length + 1
    main.starting.y <- (j * mask.y.length) - mask.y.length + 1
    
    main.ending.x = main.starting.x + mask.x.length - 1
    main.ending.y = main.starting.y + mask.y.length - 1
    
    main.section <- base.image[main.starting.x:main.ending.x,main.starting.y:main.ending.y]
    
    sub.section <- matrix ( 0 , nrow = precision , ncol = 1)
    
    #sub.section[1] <- sum(main.section)/(nrow(main.section)*ncol(main.section))
    
    for ( k in 1:precision){
      for (l in 1:precision){
        sub.starting.x <- (k * precision) - precision + 1
        sub.starting.y <- (l * precision) - precision + 1
      
        sub.ending.x <- sub.starting.x + precision - 1
        sub.ending.y <- sub.starting.y + precision - 1
        
        sub.sum <- sum (main.section[sub.starting.x:sub.ending.x,sub.starting.y:sub.ending.y] )
        sub.section[((k * precision) - (precision - 1)) + (l-1) ] <- sub.sum / precision^2        
      }
    }
    if ( max(sub.section) > 0 ){
     # sub.section <- sub.section / max(sub.section)
      sub.section <- round ( sub.section , 5 )
    }
    
    differences <- t(apply(characters.matrix[,-1], 1 , function(x){sqrt(abs( x^2 - sub.section^2))}))
    differences <- data.frame ( differences = apply ( differences , 1 , sum) )
    differences <- data.frame( characters = characters.matrix$characters ,differences =  differences$differences , stringsAsFactors = FALSE)
    output.character <- differences$characters[differences[,2] == min(differences[,2])][1]
    
    output.matrix[i,j] <- output.character
    
    
  }
}

write.table(output.matrix,file=paste0(outputfile , ".txt" ),sep="",quote=FALSE , row.names = FALSE , col.names = FALSE)

