# Function calls
# counting
expectedColour <- function(pak) {
  red <- summary(pak$suit)[2] + summary(pak$suit)[3]
  black <- summary(pak$suit)[1] + summary(pak$suit)[4]
  COLOUR <- (red >=black)
  return(COLOUR)
}

expectedRank <- function(pak, card)
{
  cardhigher = sum(pak$rank >= card1$rank)
  cardlower = sum(pak$rank < card1$rank)
  HIGHER = cardhigher>=cardlower
  return(HIGHER)
}

expectedIn <- function(card1, card2, pak)
{
  if (card1$rank < card2$rank)
  {
    cardsoutside= sum(pak$rank<=card1$rank) + sum(pak$rank>=card2$rank)
    cardsinside = dim(pak)[1] - cardsoutside
    INSIDE = cardsinside >= cardsoutside
  } else
  {
    cardsoutside= sum(pak$rank<=card2$rank) + sum(pak$rank>=card1$rank)
    cardsinside = dim(pak)[1] - cardsoutside
    INSIDE = cardsinside >= cardsoutside
  }
  return(INSIDE)
}

expectedHon <- function(card3, pak)
{
  HONNOUR <- sum(pak$rank>10) > sum(pak$rank<11)
  return(HONNOUR)
}

suit <- c("C" ,"D", "H","S")
ranking <- c(2:9, "T", "J", "Q", "K", "A")
rank <- c(1:13)
deck <- data.frame(expand.grid(suit,rank))
colnames(deck) <- c("suit", "rank")

r <- 100000
stappen <- matrix(1,r)

for (i in 1:r) {
  pak <- deck
  
  proceed1 <- FALSE
  proceed2 <- FALSE
  proceed3 <- FALSE
  proceed4 <- FALSE
  draws <- 1
  stap <- 1
  
  
  kaarten1 = NULL
  kaarten2 = NULL
  kaarten3 = NULL
  
  while (!proceed1) 
  {
    # make a guess
    guessColour <- expectedColour(pak)
    
    # draw a card
    number <- sample(1:nrow(pak),1)
    card1 <- pak[number,]
    pak <- pak[-number,]
    
    kaarten1[[draws]] <- card1
    correctR <- guessColour && (card1$suit== "D" | card1$suit == "H")
    correctB <- (!guessColour) && (card1$suit== "C" | card1$suit == "S")
    
    if  (correctR | correctB) 
    {
      proceed1 <- TRUE
    }  else  
    {
      stap = 1
      break
    } 
  }
  
  while (!proceed2 && proceed1)
  {
    gok <- expectedRank(card1)
    
    # draw a card
    number <- sample(1:nrow(pak),1)
    card2 <- pak[number,]
    pak <- pak[-number,]
    kaarten2[[draws]] <- card2
    
    stap <- 2
    correctL <- (!gok && card1$rank > card2$rank)
    correctH <- (gok && card1$rank < card2$rank)
    equal <- card1$rank == card2$rank
    
    if (correctL | correctH)
    {
      proceed2 <- TRUE
      
    } else if (!equal)
    {
      stap <- 2 
      break
    }
  }
  
  while(!proceed3 && proceed2 && proceed1)
  {
    # draw a card
    number <- sample(1:nrow(pak),1)
    card3 <- pak[number,]
    pak <- pak[-number,]
    kaarten3[[draws]] <- card3
    
    correctO1 <- correctO2 <- correctI1 <- correctI2 <- FALSE
    
    stap <- 3
    draws <- draws + 1
    guessIn<- expectedIn(card1,card2,pak)
    equal <- (card3$rank == card2$rank | card3$rank == card1$rank)
    if (card1$rank < card2$rank)
    {
      correctO1 <- !guessIn && (card3$rank < card1$rank | card3$rank > card2$rank)
      correctI1 <- guessIn && (card3$rank > card1$rank | card3$rank < card2$rank)
      if (correctO1 | correctI1)
      {
        proceed3 <- TRUE
        stap <- 4
      } else
      {stap <- 3
      break }
    } else if (card1$rank > card2$rank)
    {
      correctO2 <- !guessIn && (card3$rank < card2$rank | card3$rank > card1$rank)
      correctI2 <- guessIn && (card3$rank > card2$rank | card3$rank < card1$rank)
      if (correctO2 | correctI2)
      {
        proceed3 <- TRUE
        stap <- 4
      } else {
        stap <- 3
        break}
      
    } else if (!equal)
    {
      proceed3 <- TRUE
      proceed4 <- TRUE
      stap <- 3
    }
    
  }    
  proceed5 <- FALSE
  while(proceed3 & !proceed5)
  {
    # have a guess
    guessHon <- expectedHon(card3, pak)
    
    # draw a card
    number <- sample(1:nrow(pak),1)
    card4 <- pak[number,]
    
    if (card4$rank > 11 & guessHon)
    {stap <- 5
    proceed5 <- TRUE } else if (card4$rank < 11 & !guessHon)
    {
      stap <- 5
      proceed5 <- TRUE
    } else
    {stap = 4
    proceed5 <- TRUE
    
    }
  }
  stappen[i] <- stap
}


