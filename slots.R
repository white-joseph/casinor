spin_slots <- function(symbols, probs){
  sample(symbols, 3, replace = TRUE, prob = probs)
}

play_slots <- function(symbols, probs){
  spin <- spin_slots(symbols, probs)
  
  n_cherry <- sum(spin == "cherry")
  n_lemon <- sum(spin == "lemon")
  n_bar <- sum(spin == "bar")
  n_seven <- sum(spin == "seven")
  n_horseshoe <- sum(spin == "horseshoe")
  n_crown <- sum(spin == "crown")
  n_bell <- sum(spin == "bell")
  n_clover <- sum(spin == "clover")
  
  if (n_cherry == 3) {
    payout <- 2
  } else if (n_lemon == 3) {
    payout <- 5
  } else if (n_bar == 3) {
    payout <- 10
  } else if (n_seven == 3) {
    payout <- 100 
  } else if (n_horseshoe == 3) {
    payout <- 50
  } else if (n_crown == 3) {
    payout <- 90
  } else if (n_bell == 3) {
    payout <- 80
  } else if (n_clover == 3) {
    payout <- 75
  } else {
    payout <- -1
  }

  return(list(
    spin = spin,
    winnings = payout
  ))
}

simulate_slots <- function(symbols, probs, runs) {
  results <- rep(NA, runs)
  spins <- matrix(NA, nrow = runs, ncol = 3)
  
  for (i in seq_len(runs)) {
    result <- play_slots(symbols, probs)
    results[i] <- result$winnings
    spins[i,] <- result$spin
  }
  
  profit <- sum(results)

  return(list(
    runs = runs,
    spins = spins,
    profit = profit
  ))
}

play_slots_1 <- function(symbols, probs, payouts) {
  spin <- spin_slots(symbols, probs)
  
  names(payouts) <- symbols
  jackpot <- (length(unique(spin)) ==  1)
  
  if (jackpot) {
    symbol <- spin[1]
    profit <- payouts[symbol]
  } else {
    symbol <- NA
    profit <- -1
  }
  
  return(list(
    jackpot = jackpot,
    symbol = symbol,
    profit = profit
  ))
}

