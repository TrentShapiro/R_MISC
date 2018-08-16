
#### Plots

plot.card <- function (card , player , card.number , table){
  
  generic.card <- data.frame(x = c(430,460,460,430), y=c(40,40,105,105))
  
  if ( regexpr("Hand 2" , table$PlayerNames[player])>0 ){
    player = player - 1.50
  }
  
  player.offset = (1 - player) * 100
  
  generic.card$x <- generic.card$x + player.offset
  
  card.offset.x <- 10 * (card.number) - 30
  card.offset.y <- 40 - ((card.number - 1) * 20)

  
  
  generic.card$x <- generic.card$x + card.offset.x
  generic.card$y <- generic.card$y + card.offset.y
  
  
  polygon ( x = generic.card$x , y = generic.card$y , col="white") +
    text.default(x = c(generic.card$x[2]-8,generic.card$x[4]+8) , y = c(generic.card$y[2]+10, generic.card$y[4]-10), labels = as.character(card))
}

plot.dealer.card <- function (card , card.number , show.flag = 0){
  
  card.offset.x <-  (card.number - 1) * 33
  generic.card <- data.frame(x = c(200,230,230,200), y=c(310,310,375,375))
  
  generic.card$x <- generic.card$x + card.offset.x
  
  if (card.number == 1 && show.flag == 0){
    p <- polygon ( x = generic.card$x , y = generic.card$y , col="navy")
  }else{
    p <- polygon ( x = generic.card$x , y = generic.card$y , col="white") +
      text.default(x = c(generic.card$x[2]-8,generic.card$x[4]+8) , y = c(generic.card$y[2]+10, generic.card$y[4]-10), labels = as.character(card))
  }
  p
}


#### Calculations
create.deck <- function ( numDecks ){
  Faces <- rep( c("A","K","Q", "J"), numDecks )
  Nums <- rep ( c(2,3,4,5,6,7,8,9,10) , numDecks )
  deck <- c(Faces , Nums)
  return (deck)
}

shuffle.deck <- function ( deck ){
    deck2 <- sample ( 1 : length(deck) , length(deck) , replace=F )
    deck2 <- deck[deck2]
    return(deck2)
}

deal.deck <- function (deck , numplayers = 1 , minbet = 25 , running.totals){
  if ( length (deck) <= 4 * (numplayers+1) ){
    print ( "There are not enough cards to play a round, please start a new deck.")
    break
  } else if (numplayers < 1){
    print ( "Dealer cannot play alone!")
    break
  }
  
  burn <- deck[1]
  deck <- deck[2:length(deck)]

  table <- data.frame(PlayerNames = rep(0,numplayers+1),
                      Card1 = rep(0,numplayers+1),
                      Card2 = rep(0,numplayers+1),
                      Card3 = rep(0,numplayers+1),
                      Card4 = rep(0,numplayers+1),
                      Card5 = rep(0,numplayers+1),
                      Bet = rep(0,numplayers+1),
                      Winnings = rep(0,numplayers+1),
                      InOut = rep(1,numplayers+1))
  
#  print ( table )

  
  
  Sys.sleep(1)
  
  for ( i in 1:numplayers){
    table$PlayerNames[i]<- running.totals$Players[i]
    table$Card1[i] <- deck[1]
    plot.card(deck[1],i,1,table)
    table$Bet[i] <- minbet
    deck <- deck[2:length(deck)]
    
#    table.hidden <- table
#    print ( table.hidden )
    Sys.sleep(1)
  }
  
  table$PlayerNames[numplayers+1] <- "Dealer"
  table$Card1[numplayers+1] <- deck[1]
  plot.dealer.card(deck[1],1,0)
  deck <- deck[2:length(deck)]
#  table.hidden <- table
#  table.hidden$Card1[numplayers+1] <- "Hole"
  
#  print ( table.hidden )
  Sys.sleep(1)
  
  burn <- append ( burn , deck[1] , length(burn) )
  deck <- deck[2:length(deck)]
  
  for ( i in 1:numplayers){
    table$Card2[i] <- deck[1]
    plot.card(deck[1],i,2,table)
    deck <- deck[2:length(deck)]
#    table.hidden <- table
#    table.hidden$Card1[numplayers+1] <- "Hole"
#    print ( table.hidden )
    Sys.sleep(1)
  }
  table$Card2[numplayers+1] <- deck[1]
  plot.dealer.card(deck[1],2)
  deck <- deck[2:length(deck)]
  Sys.sleep(1)
  
#  table.hidden <- table
#  table.hidden$Card1[numplayers+1] <- "Hole"
#  print ( table.hidden )
  return ( table )
}

play.surrender <- function ( table , player){
  print(paste("Player",player," has Surrendered"))
  Sys.sleep(1)
  table$InOut[player] <- 0
  table$Winnings[player] <- as.numeric(table$Bet[player])/2
  table$Bet[player] <- 0
  
  if ( table$PlayerNames[player] != "Dealer"){
    #    table.hidden <- table
    #    table.hidden$Card1[length(table$PlayerNames)] <- "Hole"
    #    print ( table.hidden )
    Sys.sleep(1)
    print(paste("Play goes to ", table$PlayerNames[player+1]))
  }else{
    #    print (table)
    Sys.sleep(1)
  }
  
  return ( table )
}

play.stay <- function (table , player){
    
  print(paste(table$PlayerNames[i]," Stands"))
  Sys.sleep(1)
    
  if ( table$PlayerNames[player] != "Dealer"){
#    table.hidden <- table
#    table.hidden$Card1[length(table$PlayerNames)] <- "Hole"
#    print ( table.hidden )
    Sys.sleep(1)
    print(paste("Play goes to ", table$PlayerNames[player+1]))
  }else{
#    print (table)
    Sys.sleep(1)
  }
  
  return ( table )
}

play.hit <- function ( table , player , deck){
  print(paste(table$PlayerNames[player]," Hits"))
  Sys.sleep(1)
  dealer.flag = 0
  if ( table$PlayerNames[player] == "Dealer"){
    dealer.flag = 1
  }
  
  if ( table$Card2[player] == 0 ){
    table$Card2[player] <- deck[1]
    if ( dealer.flag == 0){
      plot.card(deck[1],player,2,table)
    }else{
      plot.dealer.card(deck[1],2)
    }
  }else if ( table$Card3[player] == 0){
    table$Card3[player] <- deck[1]
    if ( dealer.flag == 0){
      plot.card(deck[1],player,3,table)
    }else{
      plot.dealer.card(deck[1],3)
    }
  }else if (table$Card4[player] == 0){
    table$Card4[player] <- deck[1]
    if ( dealer.flag == 0){
      plot.card(deck[1],player,4,table)
    }else{
      plot.dealer.card(deck[1],4)
    }
  }else if (table$Card5[player] == 0){
    table$Card5[player] <- deck[1]
    if ( dealer.flag == 0){
      plot.card(deck[1],player,5,table)
    }else{
      plot.dealer.card(deck[1],5)
    }
  }
  Sys.sleep(1)
  
  current.hand <- table[player, colnames(table)[regexpr( 'Card' , colnames(table) ) > 0 ]]
  
  current.hand.numeric <- card.convert ( current.hand )
  
  if ( current.hand.numeric > 21 ){
    table$InOut[player] <- 0
    table$Winnings[player] <- -1 * as.numeric ( table$Bet[player] ) 
    table$Winnings[table$PlayerNames == "Dealer"] <- table$Bet[player]
    table$Bet[player] <- 0
    print(paste(table$PlayerNames[player]," BUSTS!!"))
    if ( table$PlayerNames[player] != "Dealer"){
      print(paste("Play goes to ", table$PlayerNames[player+1]))
    }
    
  }
  
  if ( table$PlayerNames[player] != "Dealer"){
#    Sys.sleep(1)
#    table.hidden <- table
#    table.hidden$Card1[length(table$PlayerNames)] <- "Hole"
#    print ( table.hidden )
    Sys.sleep(1)
  }else{
#    print (table)
    Sys.sleep(1)
  }
  
  return ( table )
}

play.double <- function ( table , player , deck){
  print(paste(table$PlayerNames[player]," DOUBLES!"))
  Sys.sleep(1)
  
  table$Bet[player] <- 2 * as.numeric ( table$Bet[player] )
  
  table <- play.hit ( table , player , deck)
  
  if ( table$PlayerNames[player] != "Dealer"){
    print(paste("Play goes to ", table$PlayerNames[player+1]))
  }
  Sys.sleep(1)
  
  return ( table )
}

split.convert <- function ( table ){
  for ( i in 1:(length(table$PlayerNames)-1)){
    if ( regexpr("Hand 2" , table$PlayerNames[i])>0 ){
      real.name <- substring( table$PlayerNames[i] , 1 , regexpr("Hand" , table$PlayerNames[i])-2)
      
      hand2.Winnings <- table$Winnings[i]
      table$Winnings[table$PlayerNames == real.name] <- as.numeric ( table$Winnings[table$PlayerNames == real.name]) + as.numeric(hand2.Winnings)
      
      table <- rbind ( table[1:(i-1),],table[(i+1):length(table$PlayerNames),] )
    }
  }
  return (table)
}

play.split <- function ( table , player){
  print ( paste ( table$PlayerNames[player] , "splits!"))
  Sys.sleep(1)
  name <- table$PlayerNames[player]

  new.name2 <- paste(name, "Hand 2")
  new.row <- c(new.name2 , table$Card2[player] , 0 , 0 , 0 , 0 , table$Bet[player] , 0 , 1)
  table$Card2[player] <- 0
  plot.card("SP" , player , 2, table)
    
  table <- rbind (table[1:player,],new.row,table[(player+1):length(table$PlayerNames),])
  plot.card(table$Card1[player+1],player+1,1,table)
  
#  table.hidden <- table
#  table.hidden$Card1[length(table$PlayerNames)] <- "Hole"
#  print ( table.hidden )
  Sys.sleep(1)
  
  return (table)
}

play.optimal <- function ( table , deck , player){
  current.hand <- table[player, colnames(table)[regexpr( 'Card' , colnames(table) ) > 0 ]]
  current.hand.numeric <- card.convert ( current.hand )
  
  dealer.up.card <- table$Card2[table$PlayerNames=="Dealer"]
  
  
}

card.convert <- function ( hand ){
  softflag <- 0
  convertedhand <- hand
  
  for (x in 1:ncol(hand) ){
    thiscard <- hand[x]
    
    if (suppressWarnings( is.na(as.numeric(thiscard) ) ) ){
      if ( thiscard %in% c("J","Q","K")){
        convertedhand[x] <- as.numeric(10)
      } else {
        softflag = 1
        convertedhand[x] <- as.numeric(11)
      }
    } else {
      convertedhand[x] <- as.numeric(thiscard)
    }
    
  }
  
  if ( sum ( convertedhand[convertedhand < 11]) > 10 && softflag == 1){
    convertedhand[convertedhand == 11] <- 1
  }
  
  if ( sum ( convertedhand) > 21 && softflag == 1){
    convertedhand[convertedhand == 11] <- 1
  }
  
  handtotal <- sum(convertedhand)
  return(handtotal)
}

dealer.reveal <- function ( table ){
  print("The dealer reveals:")
#  print( table )
  plot.dealer.card(table$Card1[table$PlayerNames=="Dealer"],1,1)
  Sys.sleep(1)
  
  return(table)
  
}

dealer.bj.check <- function ( table ){
  Sys.sleep(1)
  current.hand <- table[table$PlayerNames == "Dealer", colnames(table)[regexpr( 'Card' , colnames(table) ) > 0 ]]
  
  current.hand.numeric <- card.convert ( current.hand )
  
  if (current.hand.numeric == 21 ){
    print ("DEALER BLACKJACK!")
    p <- plot.dealer.card ( table$Card1[table$PlayerNames=="Dealer"] , 1 , 1)
    for ( i in 1:(length(table$PlayerNames)-1)){
      table$InOut[i]<- 0
      table$Winnings[table$PlayerNames=="Dealer"] <- as.numeric (table$Winnings[table$PlayerNames=="Dealer"]) + as.numeric (table$Bet[i])
      table$Winnings[i] <- -1 * as.numeric (table$Bet[i])
      table$Bet[i] <- 0
    }
    
    
#    print ( table )
    
    Sys.sleep(1)
  } else {
    print ("Play goes to Player1")
  }
  
  return(table)
}

dealer.play <- function ( table , deck ){
  table <- dealer.reveal(table)
  
  current.hand <- table[table$PlayerNames == "Dealer", colnames(table)[regexpr( 'Card' , colnames(table) ) > 0 ]]
  
  current.value <- card.convert(current.hand)
  
  while ( current.value <= 16 && table$Card5[table$PlayerNames == "Dealer"] == 0 && sum ( as.numeric(table$InOut) ) > 1){
    table <- play.hit(table , length(table$PlayerNames) , deck)
    deck <- deck[2:length(deck)]
    current.hand <- suppressWarnings( table[table$PlayerNames == "Dealer", colnames(table)[regexpr( 'Card' , colnames(table) ) > 0 ]])
    current.value <- card.convert(current.hand)
    print ( paste ( "Dealer currently showing" , current.value) )
    Sys.sleep(1)
  }
  
  Sys.sleep(1)
  print ( "Game has ended - RESULTS:")
  Sys.sleep(1)
  
  
  for ( i in 1:(length(table$PlayerNames)- 1)){
    player.value <- card.convert ( table[i, colnames(table)[regexpr( 'Card' , colnames(table) ) > 0 ]] )
    
    if ( table$InOut[i] == 1){
      if ( table$InOut[table$PlayerNames=="Dealer"] == 0){
        print(paste(table$PlayerNames[i], " Wins $" , table$Bet[i] , " because of dealer BUST!"))
        table$Winnings[i] <- as.numeric(table$Winnings[i]) + as.numeric(table$Bet[i])
        table$Bet[i] <- 0
        Sys.sleep(1)
      }else if ( player.value == current.value ){
        print(paste(table$PlayerNames[i], " pushes $" , table$Bet[i], " with a " , player.value))
        table$Winnings[i] <- as.numeric(table$Winnings[i])
        table$Bet[i] <- 0
        Sys.sleep(1)
      }else if ( player.value > current.value ){
        print(paste(table$PlayerNames[i], " wins $" , table$Bet[i], " with a " , player.value , " vs Dealer ", current.value))
        table$Winnings[i] <- as.numeric ( table$Winnings[i] )  +  as.numeric ( table$Bet[i] )
        table$Bet[i] <- 0
        Sys.sleep(1)        
      }else if (player.value < current.value ){
        print(paste(table$PlayerNames[i], " losses $" , table$Bet[i], " with a " , player.value , " vs Dealer ", current.value))
        table$Winnings[table$PlayerNames == "Dealer"] <- table$Bet[i]
        table$Winnings[i] <- as.numeric ( table$Winnings[i] ) - as.numeric ( table$Bet[i] )
        table$Bet[i] <- 0
        Sys.sleep(1)
      }
    } else {
    print(paste(table$PlayerNames[i], " has already ressigned "))
    }
  }
  Sys.sleep(1)
  return(table)
}




##### MAIN PROGRAM
numplayers <- readline(prompt="How many players?")
minbet <- readline(prompt="What would you like the minimum bet to be?")
numplayers <- as.numeric(numplayers)
minbet <- as.numeric(minbet)
running.totals <- data.frame (Players = rep(0,numplayers), Winnings = 0)
deck <- 0;
for ( i in 1:numplayers){
  running.totals$Players[i] <- readline(prompt=paste("Name of Player" , i , "?"))
}
check.value = TRUE

while ( check.value == TRUE){
  #Set Up
  #running.totals$Winnings <- running.totals$Winnings - minbet
  print(running.totals)
  Sys.sleep(2)
  p <- 0;
  table.top <- data.frame(x = c(0,500,500,0) , y=c(0,0,400,400))
  
  plot(table.top$x, table.top$y , axes=FALSE , ann=FALSE) + 
    polygon ( x = table.top$x , y = table.top$y , col="chartreuse4")
  
  text.default(x = 230 , y = 290 , labels = "Dealer",col="white", cex = 2)
  
  for ( i in 1:numplayers){
    x.mid <- 425 - 100 * (i-1)
    p <- p + text.default( x = x.mid , y = 160 , labels = running.totals$Players[i], col= "white", cex = 2) +
      polygon (x = c(x.mid-20,x.mid+20,x.mid+20,x.mid-20) , y = c(175,175,200,200), col="white", border="black") +
      text.default ( x = x.mid , y = 187 , labels = paste("$",running.totals$Winnings[i],sep=""), cex = 1.5)
  }
  p
  
  if ( length (deck) <= 4 * (numplayers+1) ){
    deck <- create.deck(8)
    deck <- shuffle.deck(deck)
  }

  table <- deal.deck ( deck , numplayers , minbet , running.totals)
  deck <- deck[(3 + 2 * ( 1 + numplayers)) : length(deck)]
  
  for ( i in 1:numplayers){
    current.hand <- suppressWarnings( table[i, colnames(table)[regexpr( 'Card' , colnames(table) ) > 0 ]])
    current.hand.numeric <- card.convert ( current.hand )
    
    if ( current.hand.numeric == 21){
      print(paste("BLACKJACK!" , table$PlayerNames[i], "Wins $" , as.numeric ( table$Bet[i] ) * (3/2) , " "))
      table$Winnings[i] <- floor ( as.numeric(table$Winnings[i]) + (2/3) * as.numeric(table$Bet[i]) )
      table$Bet[i] <- 0
      table$InOut[i] <- 0
      Sys.sleep(1)
    }
    
  }
  
  if ( table$Card2[table$PlayerNames=="Dealer"] %in% c("A","K","Q","J","10")){
    print ( "Dealer checks for BJ")
    table <- dealer.bj.check (table)
  }

  dealer.hand <-  suppressWarnings( table[table$PlayerNames=="Dealer", colnames(table)[regexpr( 'Card' , colnames(table) ) > 0 ]] )
  dealer.value <- card.convert(dealer.hand)
  if ( dealer.value < 21 ){
    
    #Check for splits
    for ( i in 1:(length(table$PlayerNames)-1)){
     if ( table$Card1[i] == table$Card2[i]){
       split.check <- readline(prompt = paste( table$PlayerNames[i] , "would you like to split? (Y/N)"))
       if ( split.check == "Y"){
         table <- play.split ( table , i )
       }
     } 
    }
    
    
    
    #Play Game
    for ( i in 1:length(table$PlayerNames)){
      Sys.sleep(2)
      dealer.up.card <- table[length(table$PlayerNames),'Card2']
      currently.playing = TRUE
      
      
      if ( i < length(table$PlayerNames) && table$InOut[i] > 0){
        while ( currently.playing == TRUE){
          player.hand <-  suppressWarnings( table[i, colnames(table)[regexpr( 'Card' , colnames(table) ) > 0 ]] )
          player.hand.numeric <- card.convert ( player.hand )
          print(paste(table$PlayerNames[i],", You currently have a" , player.hand.numeric, " against a Dealer" , dealer.up.card))
          action <- readline(prompt = paste(table$PlayerNames[i], ": Would you like to: Hit, Stay, Double, or Surrender?"))
          
          if ( action %in% c("Hit","Stay","Double","Surrender") ){
            if (action == "Hit"){
              table <- play.hit(table , i , deck)
              deck <- deck[2:length(deck)]
              if ( table$InOut[i] == 0){
                currently.playing = FALSE
              }
            }else if (action == "Stay"){
              currently.playing = FALSE
              table <- play.stay (table , i)
            }else if (action == "Surrender"){
              currently.playing = FALSE
              table <- play.surrender ( table , i)
            }else if (action == "Double"){
              if (table$Card3[i] == 0){
                table <- play.double(table , i , deck)
                deck <- deck[2:length(deck)]
                currently.playing = FALSE
              }else{
                print("You cannot double after the first card!")
              }
            }
          }
        }
      }else{
        table <- dealer.play(table , deck)
      }
    }
  }
  
  ##### Subtract dealer cards from deck
  if ( table$Card5[length(table$Card5)] != 0 ){
    deck <- deck[4:length(deck)]
  } else if ( table$Card4[length(table$Card4)] != 0 ){
    deck <- deck[3:length(deck)]
  } else if ( table$Card3[length(table$Card3)] != 0 ){
    deck <- deck[2:length(deck)]
  }
  
  ##### CONSOLIDATE SPLITS
  if (  suppressWarnings( any ( regexpr ( "Hand 2" , table$PlayerName) > 0 ) ) ){
    table <- split.convert ( table )
  }
  
  for ( i in 1:length(running.totals$Players)){
    running.totals$Winnings[i] <- as.numeric (running.totals$Winnings[i]) + as.numeric(table$Winnings[i])
  }
  
  print(running.totals)
  
  ##### Plot running totals
  for ( i in 1:numplayers){
    x.mid <- 425 - 100 * (i-1)
    p <- p + text.default( x = x.mid , y = 160 , labels = running.totals$Players[i], col= "white", cex = 2) +
      polygon (x = c(x.mid-20,x.mid+20,x.mid+20,x.mid-20) , y = c(175,175,200,200), col="white", border="black") +
      text.default ( x = x.mid , y = 187 , labels = paste("$",running.totals$Winnings[i],sep=""), cex = 1.5)
  }
  p
  
  userinput <- readline ( prompt = "Would you like to keep playing? Y/N")
  
  if (userinput == "N"){
    check.value = FALSE
    #dev.off()
  }
  
}

