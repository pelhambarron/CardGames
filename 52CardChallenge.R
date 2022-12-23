DecisionRule <- function(x){
tiebreaker = sample(0:1,1)
    
  if(x<8){
    return("higher")
  }else if (x>8){
    return("lower")
  }else if (tiebreaker == 0){
    return("lower")
  }else {return("higher")
  }
}

DecisionChecker <- function(card0,card1,decision){
  
  card_dif = card1-card0
  
  if(decision == "higher" & card_dif>0){
      return (1)
    }else if (decision == "higher" & card_dif<0){
      return (-1)
    }else if (decision == "lower" & card_dif>0){
      return (-1)
    }else if (decision == "lower" & card_dif<0){
      return (1)
    }else {return(0)
    }
}
 


###Simulation
N <- 10000000  #Number of plays  ## 10 Million
C <- 5 #Number of consecutive calls required to win
SimulationResults<-c()  #Vector of Simulation results

###

for (i in 1:N){
  
  ### Step 1, Shuffle the pack of playing cards
  
  deck <- ceiling(sample(5:56, 52, replace = F)/4)   ### 5-56 because Ace is high
  winning = 1
  c=1   ## Starting card in the deck
  r = 0  ## #repeat numbers drawn
  z = c-r  ##nth card AKA #calls +1 hence z<=C condition
  while(winning == 1 & z<=C){
    card = deck[c]
    choice = DecisionRule(card)
    card1 = deck[c+1]
    
    correct = DecisionChecker(card,card1,choice)
    
    if(correct >= 0){
      c = c+1
    }else if (correct <0){
      winning = 0
    }
    
    if(correct == 0){
      r = r+1
    }
    
    z = c-r
    
  }
  
 SimulationResults[i]<- winning 
  
}


t<- table(SimulationResults)

t[2]/N

####   probability of winning is ~ 0.23