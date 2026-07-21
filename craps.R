#' Roll two fair die
#'
#' Rolls two fair die and returns an atomic vector of each roll
#' 
#' @return A numeric vector of length 3 where the first 2 elements are the roll 
#' of the respective die, and element 3 is their sum.
#'  
#' @examples 
#' roll_die()
#' roll_die()
#' 
#' @export

roll_die <- function() {
  die = c(1:6)
  
  roll <- sample(die, 2, replace =TRUE)
  roll <- c(roll, sum(roll))
  names(roll) <- c("die1", "die2", "sum")
  
  return(roll)
}

come_out_roll <- function() {
  
  roll <- roll_die()
  
  if (roll["sum"] %in% c(7,11)) {
    outcome <- "natural"
    point <- NA
  } else if (roll["sum"] %in% c(2,3)) {
    outcome <- "craps"
    point <- NA
  } else if (roll["sum"] == 12) {
    outcome <- "push"
    point <- NA
  } else {
    outcome <- "point"
    point <- roll[["sum"]]
  }
  
  return(list(
    roll = roll,
    outcome = outcome,
    point = point
  ))
}

#' Rolls a die until their sum is 7 or the point value to resolve the point
#' 
#' Rolls two fair die until either their sum is 7 or the established point. Returns
#' a matrix where each row is a specific roll. Returns a numeric vector of length 1
#' which is the number of rolls it took to resolve the point.
#' 
#' @param point Whole numeric vector of length 1 who's value must be between 2 and 12 but not 7
#' 
#' @examples 
#' resolve_point(4)
#' resolve_point(6)
#' 
#' @export

resolve_point <- function(point) {
  
  if (!is.numeric(point) || length(point) != 1) {
    stop("point must be numeric and length 1.")
  }
  
  if (point < 2 || point > 12 || point == 7) {
    stop("point must be between 2 and 12 but not 7.")
  }
  
  roll <- roll_die()
  n_rolls <- 1
  rolls <- matrix(roll, nrow = 1)
  colnames(rolls) <- c("die1", "die2", "sum")
  
  while (roll["sum"] != 7 && roll["sum"] != point) {
    roll <- roll_die()
    n_rolls <- n_rolls + 1
    rolls <- rbind(rolls, roll)
    rownames(rolls) <- NULL
  }
  
  if (rolls[nrow(rolls), "sum"] == 7) {
    outcome <- "seven_out"
  } else {
    outcome <- "point_made"
  }
  
  return(list(
    rolls = rolls,
    outcome = outcome,
    n_rolls = n_rolls
  ))
}

pass_line_profit <- function(outcome, pass_line_wager) {
  
  if (outcome %in% c("natural", "point_made")) {
    profit <- pass_line_wager
  } else if (outcome %in% c("craps", "push", "seven_out")) {
    profit <- -pass_line_wager
  }
  
  return(profit)
}

dont_pass_profit <- function(outcome, dont_pass_wager) {
  
  if (outcome %in% c("craps", "seven_out")) {
    profit <- dont_pass_wager
  } else if (outcome %in% c("natural", "point_made")) {
    profit <- -dont_pass_wager
  } else if (outcome == "push") {
    profit <- 0
  }
  
  return(profit)
}

odds_profit <- function(point, outcome, odds_wager) {
  
  if (outcome == "point_made"  && point %in% c(4,10)) {
    profit <- 2*odds_wager
  } else if (outcome == "point_made" && point %in% c(5,9)) {
    profit <- (3/2)*odds_wager
  } else if (outcome == "point_made" && point %in% c(6,8)) {
    profit <- (6/5)*odds_wager
  } else {
    profit <- -odds_wager
  }
  
  return(profit)
}

play_craps <- function(pass_line_wager = 10, dont_pass_wager = 0, odds_wager = 0) {
  come_out <- come_out_roll()
  
  if(come_out$outcome != "point") {
    
    pass_line <- pass_line_profit(come_out$outcome, pass_line_wager)
    dont_pass <- dont_pass_profit(come_out$outcome, dont_pass_wager)
    
    profit <- sum(pass_line, dont_pass)
    
    return(list(
      roll = come_out$roll,
      outcome = come_out$outcome,
      profit = profit
    ))
  }
  
  resolution <- resolve_point(come_out$point)
  rolls <- resolution$rolls
  
  pass_line <- pass_line_profit(resolution$outcome, pass_line_wager)
  dont_pass <- dont_pass_profit(resolution$outcome, dont_pass_wager)
  odds <- odds_profit(come_out$point, resolution$outcome, odds_wager)
  
  profit <- sum(pass_line, dont_pass, odds)
  
  return(list(
    roll = come_out$roll,
    point = come_out$point,
    rolls = rolls,
    outcome = resolution$outcome,
    profit = profit
  ))
}

