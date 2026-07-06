spin_slots <- function(payouts, probs, reels = 3) {
  # Takes a named vector of slot payouts and returns a random spins
  # 
  # Parameters: 
  # 
  # payouts: a named vector where names correspond to the slot symbol and payout
  #          is the corresponding payout for that symbol
  #
  # probs: the probability correspoinding to the symobl. How likely that smybol
  #        will be chosen
  #
  # reels: the number of reels or windows on the slot machine. Default is 3.
  #
  # Returns: 
  #
  # random vector of the named symbols of payouts
  
  sample(names(payouts), reels, replace = TRUE, prob = probs)
}

play_slots <- function(payouts, probs, wager = 1, reels = 3) {
  spin <- spin_slots(payouts, probs, reels)
  
  jackpot <- (length(unique(spin)) ==  1)
  
  if (jackpot) {
    symbol <- spin[1]
    profit <- payouts[[symbol]]
  } else {
    symbol <- NA
    profit <- -wager
  }
  
  return(list(
    jackpot = jackpot,
    spin = spin,
    symbol = symbol,
    profit = profit
  ))
}

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