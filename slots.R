symbols <- c("cherry",
             "lemon",
             "bar",
             "seven",
             "horseshoe",
             "crown",
             "bell",
             "clover")

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

simulate_slots <- function(runs) {
  results <- rep(NA, runs)
  spins <- matrix(NA, nrow = runs, ncol = 3)
  
  for (i in seq_len(runs)) {
    result <- play_slots()
    results[i] <- result$winnings
    spins[i,] <- result$spin
  }
  
  total_payout <- sum(results)

  return(list(
    runs = runs,
    spins = spins,
    total_payout = total_payout
  ))
}

simulate_slots(100)

