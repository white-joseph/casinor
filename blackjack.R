face <- c(as.character(2:10), "jack", "queen", "king", "ace")
suit = c("clubs", "diamonds", "hearts", "spades")

deck <- expand.grid(
  face = face,
  suit = suit,
  stringsAsFactors = FALSE
  )

deck$value <- rep(2:14, times = 4)

print(deck)

deal <- function() {
  hand <- deck[sample.int(nrow(deck), 2),1:2]
  
  return(hand)
}

deal()

play_blackjack <- function(){
  dealer <- deal()
  player <- deal()
  
  tree <- "Dealers Cards"
  frog <- "Players Cards"
  
  return(list(
    tree <- dealer,
    frog <- player
  ))
}

play_blackjack()
