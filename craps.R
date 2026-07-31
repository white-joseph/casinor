#' Roll two fair dice
#'
#' Rolls two fair dice and returns an atomic vector of each roll
#' 
#' @return A numeric named vector of length 3 where the first 2 elements are the roll 
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

#' Rolls the come out roll of a craps game
#'
#' Rolls two dice, computes the sum. If the sum is a 7 or 11 the outcome is natural and point is NA.
#' If the sum is a 2 or 3 the outcome is craps and point is NA. If the sum is a 12 then the outcome is 
#' push and the point is NA. Else, the outcome is point and the point is the sum.
#' 
#' @return returns a list where roll is the named vector of length 3 and the first two
#' elements are the pips of each die and the third element is their sum.
#' Outcome is a character string of the respective outcome. Point is the established point if not NA.
#' 
#' @examples
#' come_out_roll()
#' come_out_roll()
#' 
#' @export

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

#' Rolls two dice until their sum is 7 or the point value to resolve the point
#' 
#' Rolls two fair dice until either their sum is 7 or the established point. Returns
#' a matrix where each row is a specific roll. Returns the rolls, the outcome, and the number 
#' of rolls it took to resolve the point. 
#' 
#' @param point Whole numeric vector of length 1 whose value must be between 2 and 12 but not 7
#' 
#' @return Returns rolls, a matrix where each row corresponds to a roll. Returns outcome, which
#' is seven_out if the loop ends on a sum of 7 or point_made if the loop ends with the point
#' being rolled again. Returns n_rolls which is the number of rolls it took to resolve 
#' the point
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

#' Calculates the profit on the pass line bet
#' 
#' Evaluates the outcome of the come out roll or the point resolution and returns
#' the profit based on the respective outcome, payout and wager.
#' 
#' @param outcome character string vector of length 1. It is the outcome of either
#' the come out phase or point resolution phase of craps. 
#' @param pass_line_wager whole positive numeric vector of length 1 for how much
#' to bet on the pass line of a craps game.
#' 
#' @return numeric vector of length 1 which is the profit on the pass line bet.
#' 
#' @examples
#' pass_line_profit("natural", 10)
#' pass_line_profit("craps", 1250)
#' 
#' @export

pass_line_profit <- function(outcome, pass_line_wager) {
  
  if (!(outcome %in% c("natural", "craps", "push", "point", "seven_out", "point_made"))) {
    stop("not a possible craps outcome.")
  }
  
  if (length(pass_line_wager) != 1) {
    stop("length of pass line wager is ", length(pass_line_wager), " but must be length 1.")
  }
  
  if (!is.numeric(pass_line_wager) || pass_line_wager < 0) {
    stop("pass line wager must be numeric and non-negative.")
  }
  
  if (pass_line_wager %% 1 != 0) {
    stop("pass line wager must be a whole number.")
  }
  
  if (outcome %in% c("natural", "point_made")) {
    profit <- pass_line_wager
  } else if (outcome %in% c("craps", "push", "seven_out")) {
    profit <- -pass_line_wager
  }
  
  return(profit)
}

#' Calculates the profit on the don't pass line bet
#' 
#' Evaluates the outcome of the come out roll or the point resolution and returns
#' the profit based on the respective outcome, payout and wager.
#' 
#' @param outcome character string vector of length 1. It is the outcome of either
#' the come out phase or point resolution phase of craps. 
#' @param dont_pass_wager whole positive numeric vector of length 1 for how much
#' to bet on the don't pass line of a craps game.
#' 
#' @return numeric vector of length 1 which is the profit on the don't pass line bet.
#' 
#' @examples
#' dont_pass_profit("natural", 10)
#' dont_pass_profit("craps", 1250)
#' 
#' @export

dont_pass_profit <- function(outcome, dont_pass_wager) {
  
  if (!(outcome %in% c("natural", "craps", "push", "point", "seven_out", "point_made"))) {
    stop("not a possible craps outcome.")
  }
  
  if (length(dont_pass_wager) != 1) {
    stop("length of dont pass line wager is ", length(dont_pass_wager), " but must be length 1.")
  }
  
  if (!is.numeric(dont_pass_wager) || dont_pass_wager < 0) {
    stop("dont pass line wager must be numeric and non-negative.")
  }
  
  if (dont_pass_wager %% 1 != 0) {
    stop("dont pass line wager must be a whole number.")
  }
  
  if (outcome %in% c("craps", "seven_out")) {
    profit <- dont_pass_wager
  } else if (outcome %in% c("natural", "point_made")) {
    profit <- -dont_pass_wager
  } else if (outcome == "push") {
    profit <- 0
  }
  
  return(profit)
}

#' Calculates the profit of taking odds on the pass line
#' 
#' Evaluates the point and outcome from the point resolution phase of craps,
#' and calculates it's respective profit. Payouts for 4 & 10: 2/1, Payouts for 
#' 5 & 9: 3/2, Payouts for 6 & 8: 6/5. Returns profit on the odds bet.
#' 
#' @param point numeric vector of length 1. Is the established point from the come out roll.
#' @param outcome character string vector of length 1. It is the outcome of either
#' the come out phase or point resolution phase of craps. 
#' @param odds_wager whole positive numeric vector of length 1 for how much
#' is bet on taking odds on the pass line. Must be a numeric and length 1, and multiple of 2 or 5.
#' 
#' @return numeric vector of length 1 which is the profit on the pass line odds bet.
#' 
#' @examples
#' odds_profit(5, "point_made", 10)
#' odds_profit(6, "seven_out", 15)
#' odds_profit(10, "point_made", 10)
#' 
#' @export

odds_profit <- function(point, outcome, odds_wager) {
  
  if (!is.numeric(point) || length(point) != 1) {
    stop("point must be numeric and length 1.")
  }
  
  if (point < 2 || point > 12 || point == 7) {
    stop("point must be between 2 and 12 but not 7.")
  }
  
  if (!(outcome %in% c("natural", "craps", "push", "point", "seven_out", "point_made"))) {
    stop("not a possible craps outcome.")
  }
  
  if (!is.numeric(odds_wager) || odds_wager < 0) {
    stop("odds wager must be numeric and non-negative.")
  }
  
  if (odds_wager %% 2 != 0 && odds_wager %% 5 != 0) {
    stop("odds wager must be a whole number and multiple of 2 or 5.")
  }
  
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