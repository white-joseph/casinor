wheel <- data.frame(
  outcome = c("0", "00", as.character(1:36)),
  color = c("green","green", "red","black","red","black","red","black","red",
             "black","red","black","red","black","red","black","red","black","red",
             "black","red","black","red","black","red","black","red","black","red",
             "black","red","black","red","black","red","black","red","black"
  )
)

spin <- wheel[sample.int(nrow(wheel), size = 1),]
spin

color_roulette <- function(wager, value) {
  color = c("green","green",
            "red","black","red","black","red","black","red",
            "black","red","black","red","black","red","black","red","black","red",
            "black","red","black","red","black","red","black","red","black","red",
            "black","red","black","red","black","red","black","red","black"
  )
  
  spin = sample(color, size = 1)
  
  if (spin == value) {
    outcome <- "win"
    profit <- wager
  } else if (spin != value) {
    outcome <- "loss"
    profit <- -wager
  }
  
  return(list(
    spin = spin,
    bet = wager,
    outcome = outcome, 
    profit = profit
  ))
}

even_odd_roulette <- function(wager, value) {
  pockets <- c("0", "00", as.character(1:36))
  spin = sample(pockets, size = 1)
  
  if (spin == "0" | spin == "00"){
    spin_type <- "green"
  } else if (as.numeric(spin) %% 2 == 0){
    spin_type <- "even"
    } else {
      spin_type <- "odd"
    }
  
  if (spin_type == value){
    outcome <- "win"
    profit <- wager
  } else if (spin_type != value) {
    outcome <- "loss"
    profit <- -wager
  }
  
  return(list(
    spin = spin,
    bet = wager,
    spin_type = spin_type,
    outcome = outcome,
    profit = profit
  ))
}

even_odd_roulette(1, "even")

straight_up_roulette <- function(wager, value){
  pockets <- c("00", "0", as.character(1:36))
  spin <- sample(pockets, size = 1)
  
  if (spin == value) {
    outcome <- "win"
    profit <- wager*38
  } else {
    outcome <- "loss"
    profit <- -wager
  }

  return(list(
    spin = spin,
    bet = wager,
    outcome = outcome,
    profit = profit
  ))
}

straight_up_roulette(1, 29)