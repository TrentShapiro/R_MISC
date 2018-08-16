#######
# Langton's Ant
#######

##Initialization
ant_init <- function (m,n,num_ants){
  m <<- m
  n <<- n
  board <- matrix(rep(0,m),nrow=m,ncol=n)
  ants <- data.frame("ant"=0,"xcord"=0,"ycord"=0,"heading"=0)
  headings <- c('left','up','right','down')
  
  for (i in 1:num_ants){
    ants [i,1] <- i
    # x-Cordinate #########
    a <- as.integer(readline(prompt = paste0("x-coordinate for ant",paste(i)," ?")))
    while(a > m | a < 0){
      a <- readline(prompt = paste0("Out of bounds! x-coordinate for ant",paste(i)," ?"))
      if(a == "break"){
        break
      }
    }
    
    ants[i,2] <- a
    
    # y-cordinate #########
    b <- as.integer(readline(prompt = paste0("y-coordinate for ant",paste(i)," ?")))
    while(b > n | b < 0){
      b <- readline(prompt = paste0("Out of bounds! y-coordinate for ant",paste(i)," ?"))
      if(b == "break"){
        break
      }
    }
    ants[i,3] <- b
    
    # Add ant to board state
    board[a,b] <- 1
    
    # heading ############
    a <- readline(prompt = paste0("heading for ant",paste(i)," ? (left, up, right, down)" ))
    while((a %in% headings) != TRUE){
      a <- readline(prompt = paste0("Out of bounds! heading for ant",paste(i)," ? (left, up, right, down)"))
      if(a == "break"){
        break
      }
    }
    
    ants[i,4] <- a
  }
  
  out <- list(board = board,ants = ants) 
  
  
}

##Update Ants' positions
update <- function(board,ants){
  
  for (i in 1:max(ants$ant)){
    if(ants$heading[i] == "left"){
      ants$heading[i] <- ifelse(board[ants$xcord[i]-1,ants$ycord[i]]==0,'up','down');
      board[ants$xcord[i],ants$ycord[i]] <- ifelse(board[ants$xcord[i],ants$ycord[i]]==0,1,0)
      ants$xcord[i] <- ants$xcord[i]-1;
      
    }else if(ants$heading[i] == "up"){
      ants$heading[i] <- ifelse(board[ants$xcord[i],ants$ycord[i]-1]==0,'right','left');
      board[ants$xcord[i],ants$ycord[i]] <- ifelse(board[ants$xcord[i],ants$ycord[i]]==0,1,0)
      ants$ycord[i] <- ants$ycord[i]-1;
      
    }else if(ants$heading[i] == "right"){
      ants$heading[i] <- ifelse(board[ants$xcord[i]+1,ants$ycord[i]]==0,'down','up');
      board[ants$xcord[i],ants$ycord[i]] <- ifelse(board[ants$xcord[i],ants$ycord[i]]==0,1,0)
      ants$xcord[i] <- ants$xcord[i]+1;
      
    }else if(ants$heading[i] == "down"){
      ants$heading[i] <- ifelse(board[ants$xcord[i],ants$ycord[i]+1]==0,'left','right');
      board[ants$xcord[i],ants$ycord[i]] <- ifelse(board[ants$xcord[i],ants$ycord[i]]==0,1,0)
      ants$ycord[i] <- ants$ycord[i]+1;
    }
    
  }
  
  out <- list(board = board,ants = ants)
}

## Main Program
ant_main <- function(x,y,nants){
  
  ants <- ant_init(x,y,nants)
  i <- 1
  
  while(1){
    ants <- update(ants$board, ants$ants)
    
    i <- i + 1;
    
    if (i %% 1000 == 0){
      Sys.sleep(0.4)
      image(ants$board)
      print(i)
    }
  }
}

## Run it (make x and y near the middle)
ant_main(150,150,1)


# x-coordinate for ant1 ?350
# y-coordinate for ant1 ?450
# heading for ant1 ? (left, up, right, down)down
# x-coordinate for ant2 ?450
# y-coordinate for ant2 ?500
# heading for ant2 ? (left, up, right, down)right
# x-coordinate for ant3 ?450
# y-coordinate for ant3 ?400
# heading for ant3 ? (left, up, right, down)up
# x-coordinate for ant4 ?350
# y-coordinate for ant4 ?350
# heading for ant4 ? (left, up, right, down)left