symbols <- c("cherry", "lemon", "bar", "seven", 
             "horseshoe", "crown", "bell", "clover")

probs <- c(
  0.35,  # cherry
  0.20,  # lemon
  0.14,  # bar
  0.04,  # seven
  0.08,  # horseshoe
  0.05,  # crown
  0.06,  # bell
  0.08   # clover
)

spin_slots <- function(symbols, probs){
  sample(symbols, 3, replace = TRUE, prob = probs)
}

play_slots <- function(){
  # I want to set something up that is like, every $1 is 1 spin, if someone wagers $50 they get 50 spins,
  spin <- spin_slots(symbols, probs)
  n_cherry <- sum(spin == "cherry")
  n_lemon <- sum(spin == "lemon")
  n_bar <- sum(spin == "bar")
  n_seven <- sum(spin == "seven")
  
  if (n_seven == 4) {
    payout = 100
  } else if (n_lemon >= 3) {
    payout = 15
  } else if (n_bar >= 3) {
    payout = 5
  } else if (n_cherry >= 3) {
    payout = 2
  } else {
    payout = 0
  }
  
  return(list(
    spin = spin,
    winnings = payout
  ))
}

simulate_slots <- function(wager) {
  payouts <- wager
  
  for (i in seq_len(wager)) {
    result <- play_slots()
    payouts[i] <- result$winnings
  }
  
  total_payout <- sum(payouts)
  profits <- total_payout - wager
  
  return(list(
    spins = wager,
    total_wagered = wager,
    total_payout = total_payout,
    profit = profits
  ))
}

simulate_slots(10000)

play_slots1 <- function(n) {
  results <- matrix(NA, nrow = n, ncol = 3)
  
  for (i in seq_len(n)) {
    result <- spin_slots(symbols, probs)
    results[i, ] <- result
  }
  
  return(results)
}

play_slots1(10)
