#' Spin a named vector of slot machine payouts
#' 
#' Takes a named vector of slot symbols and their respective payouts, 
#' returns a random symbol for a selected number of reels.
#' 
#' @param payouts Named vector of slot symbols and payouts. Name must be a character,
#' and payout must be numeric
#' @param probs A numeric vector of probabilities associated with each symbol.
#' Must sum to 1 and be equal length of payouts
#' @param reels A numeric vector of length 1 defining how many slot  reels.
#' Default is 3.
#' 
#' @return A character vector of length reels. Each element will be a name
#' from payouts.
#' 
#' @examples
#' spin_slots(payouts = c(cherry = 1.5, seven = 5), probs = c(0.8, 0.2))
#' spin_slots(payouts = c(cherry =1 , bar = 5, seven = 10), probs = c(0.7, 0.2, 0.1) , reels = 5)
#' 
#' @export

spin_slots <- function(payouts, probs, reels = 3) {
  sample(names(payouts), reels, replace = TRUE, prob = probs)
}

#' Spin a slot machine and calculate profit given a wager
#' 
#' Takes a named vector of slot symbols, the payouts for each symbol, a wager, 
#' and the number of reels. Returns a logical win, and profit if any. 
#' 
#' @param payouts Named vector of slot symbols and payouts. Name must be a character,
#' and payout must be numeric
#' @param probs A numeric vector of probabilities associated with each symbol.
#' Must sum to 1 and be equal length of payouts
#' @param wager Numeric of how much the user would like to wager. Profit is determined
#' by wager and payouts.
#' @param reels A numeric vector of length 1 defining how many slot  reels.
#' Default is 3.
#' 
#' @return A list containg jackpot, which is a logical win/loss. Symbol, which if jackpot
#' is true is the winning symbol, NA if else. Spin, the spin from the spin_slots function.
#' And profit, calculated based on the payouts times the wager. 
#' 
#' @examples 
#' play_slots(payouts = c(cherry = 1, seven = 5, bar = 15), probs = c(0.7, 0.2, 0.1))
#' play_slots(payouts = c(cherry = 5, seven = 10), probs = c(0.75, 0.25), wager = 50)
#' 
#' @export

play_slots <- function(payouts, probs, wager = 1, reels = 3) {
  spin <- spin_slots(payouts, probs, reels)
  jackpot <- (length(unique(spin)) ==  1)
  
  if (jackpot) {
    symbol <- spin[1]
    profit <- payouts[[symbol]] * wager
  } else {
    symbol <- NA
    profit <- -wager
  }
  
  return(list(
    jackpot = jackpot,
    symbol = symbol,
    spin = spin,
    profit = profit
  ))
}

#' Simulate many spins of a slot machine
#' 
#' Takes a named vector of payouts and returns the results of n spins of a slot machine.
#' 
#' @param payouts Named vector of slot symbols and payouts. Name must be a character,
#' and payout must be numeric
#' @param probs A numeric vector of probabilities associated with each symbol.
#' Must sum to 1 and be equal length of payouts
#' @param runs A numberic of how many runs of the simulation.
#' @param wager Numeric of how much the user would like to wager. Profit is determined
#' by wager and payouts.
#' @param reels A numeric vector of length 1 defining how many slot  reels.
#' Default is 3.
#' 

simulate_slots <- function(payouts, probs, runs, wager = 1, reels = 3) {
  spins <- matrix(NA, nrow = runs, ncol = reels)
  jackpots <- rep(NA, runs)
  profits <- rep(NA, runs)
  
  for (i in seq_len(runs)) {
    play <- play_slots(payouts, probs, wager, reels)
    spins[i,] <- play$spin
    jackpots[i] <- play$jackpot
    profits[i] <- play$profit
  }
  
  total_jackpots <- sum(jackpots)
  total_profit <- sum(profits)
  
  return(list(
    runs = runs,
    spins = spins,
    total_jackpots = total_jackpots,
    total_profit = total_profit
  ))
}