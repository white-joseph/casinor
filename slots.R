spin_slots <- function(payouts, probs){
  sample(names(payouts), 3, replace = TRUE, prob = probs)
}

play_slots <- function(payouts, probs) {
  spin <- spin_slots(payouts, probs)
  
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
    spin = spin,
    symbol = symbol,
    profit = profit
  ))
}

simulate_slots <- function(payouts, probs, runs) {
  results <- rep(NA, runs)
  spins <- matrix(NA, nrow = runs, ncol = 3)
  
  for (i in seq_len(runs)) {
    result <- play_slots(payouts, probs)
    results[i] <- result$jackpot
    spins[i,] <- result$spin
  }
  
  profit <- sum(results["profit"])
  
  return(list(
    runs = runs,
    spins = spins,
    profit = profit
  ))
}

simulate_slots2 <- function(payouts, probs, runs) {
  spins <- matrix(NA, nrow = runs, ncol = 3)
  
  for (i in seq_len(runs)) {
    play <- play_slots(payouts, probs)
    spins[i,] <- play$spin
    jackpot <- play$jackpot
    profit <- play$profit
  }
  
  jackpots <- sum(jackpot)
  profits <- sum(profit)
  print(jackpot)
  print(profit)
  
  return(list(
    runs = runs,
    spins = spins,
    jackpot = jackpots,
    profit = profits
  ))
}

payouts <- c("cherry" = 5, "seven" = 10, "bar" = 15)
probs <- c(0.60, 0.20, 0.20)

spin_slots(payouts, probs)
play_slots(payouts, probs)
simulate_slots2(payouts, probs, 5)
