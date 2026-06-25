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
    bet = value,
    outcome = outcome,
    profit = profit
  ))
}

straight_up_roulette(1, 29)

european_wheel <- data.frame(
  pocket = 0:36,
  color = ifelse(
    0:36 == 0, "green",
    ifelse(
      0:36 %in% c(1, 3, 5, 7, 9,
                  12, 14, 16, 18,
                  19, 21, 23, 25, 27,
                  30, 32, 34, 36),
      "red",
      "black"
    )
  ),
  
  even = ifelse(
    0:36 == 0, FALSE,
    ifelse(
      0:36 %% 2 == 0,
      TRUE,
      FALSE
    )
  )
)

spin_wheel <- function(wheel) {
  wheel[sample.int(nrow(wheel), size =1), ]
}

play_roulette <- function(wager, bet, even) {
  spin <- spin_wheel(european_wheel)
  
  if (spin$pocket == bet) {
    outcome = "win"
    profit = 35*wager
  } else if (spin$pocket != bet) {
    outcome = "loss"
    profit = -wager
  }
  
  if (spin$color == bet){
    outcome = "win"
    profit = wager
  } else if (spin$color != bet) {
    outcome = "loss"
    profit = -wager
  }
  
  if (spin$even == even){
    outcome = "win"
    profit = wager
  } else {
    outcome = "loss"
    profit = -wager
  }
  
  return(list(
    spin = spin,
    wager = wager,
    bet = bet,
    outcome = outcome,
    profit = profit
  ))
}

betting_mat <- matrix(c(1:36), nrow = 12, byrow = TRUE)
betting_mat

eveluate_bet <- function(spin, bet_type, value, wager){
  
}

play_roulette <- function(bets, wheel = european_wheel) {
  spin <- spin_wheel(wheel)
  
  # evaluate each bet using the same spin
  
  # combine the bet-level results
  
  # calculate total profit
  
  # return spin, bet results, and total profit
}