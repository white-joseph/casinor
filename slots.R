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

play_slots <- function(payouts, probs, wager = 1, reels = 3) {
  # Takes a named vector of payouts and returns if a jackpot was hit as well as the profit.
  # 
  # Parameters: 
  # 
  # payouts: a named vector where names correspond to the slot symbol and payout
  #          is the corresponding payout for that symbol
  #
  # probs: the probability corresponding to the symbol. How likely that symbol
  #        will be chosen
  #
  # wager: numeric; how much money the user wants to wager on a roll
  #
  # reels: the number of reels or windows on the slot machine. Default is 3.
  #
  # Returns: 
  #
  # list of if a jackpot occurred, what the spin was, if jackpot then the symbol, and profit.
  # If there is no jackpot, then symbol defaults to be NA. Profit = wager times payout
  #
  # example: play_slots(payouts = c("cherry" = 1, "seven" = 5, "bar" = 15), probs = c(0.7, 0.2, 0.1))
  #          may return: 
  # $jackpot
  # [1] FALSE
  
  # $spin
  # [1] "seven"  "seven"  "cherry"
  
  # $symbol
  # [1] NA
  
  # $profit
  # [1] -1
  
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

simulate_slots <- function(payouts, probs, runs, wager = 1, reels = 3) {
  # Takes a named vector of payouts, simulates n spins, and returns list of results
  # 
  # Parameters: 
  # 
  # payouts: a named vector where names correspond to the slot symbol and payout
  #          is the corresponding payout for that symbol
  #
  # probs: the probability corresponding to the symbol. How likely that symbol
  #        will be chosen
  #
  # runs: how many spins the simulation will run for
  # 
  # wager: numeric; how much money the user wants to wager on a roll. If no jackpot
  #        occurs then the profit is a loss of the value of the wager.
  #
  # reels: the number of reels or windows on the slot machine. Default is 3.
  # 
  # Returns: 
  #
  # List: numeric of the number of runs, matrix of the spin results, numeric of
  #       the number of jackpots, numeric of total payouts. Profit = wager times payout
  
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