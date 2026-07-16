#' Roll two fair die
#'
#' Rolls two fair die and returns an atomic vector of each roll
#' 
#' @return A numeric vector of length 2 where each element is an independent roll
#' 
#' @examples 
#' roll_die()
#' roll_die()
#' 
#' @export

roll_die <- function() {
  die <- c(1:6)
  roll <- sample(die, 2, replace = TRUE)
  
  return(roll)
}

#' Rolls the come out roll for a game of craps
#'
#' Rolls two fair die; Craps if their sum is 2,3, or 12. Establishes the point if
#' their sum is not craps or 7 or 11.
#' 
#' @return A list where roll is a vector of length 2 where each element is an independent roll. 
#' craps is a logical for if craps was hit, and the point is the value of the roll if the point was 
#' established. Craps FALSE if sum of the rolls are 7 and 11. Point is NA if sum of the roll is
#' 2,3,7,11 or 12.
#' 
#' @examples
#' come_out_roll()
#' come_out_roll()
#' 
#' @export

come_out_roll <- function() {
  roll <- roll_die()
  
  if (sum(roll) == 2 || sum(roll) == 3 || sum(roll) == 12) {
    craps <- TRUE
    point <- NA
  } else if (sum(roll) == 7 || sum(roll) == 11) {
    craps <- FALSE
    point <- NA
  } else {
    craps <- FALSE
    point <- sum(roll)
  }
  
  return(list(
    roll = roll,
    craps = craps,
    point = point
  ))
}

resolve_point <- function(point) {
  roll <- roll_die()
  print(paste0("Roll: ", roll[1], ", ", roll[2]))
  rolls <- matrix(roll, nrow = 1)
  n_rolls <- 1
  
  while (sum(roll) != 7 && sum(roll) != point) {
    roll <- roll_die()
    print(paste0("Roll: ", roll[1], ", ", roll[2]))
    rolls <- rbind(rolls, roll)
    n_rolls <- n_rolls + 1
    rownames(rolls) <- NULL
  }
  
  return(list(
    rolls = rolls,
    n_rolls = n_rolls
  ))
}

play_craps <- function(pass_line, dont_pass) {
  come_out <- come_out_roll()
  
  if (come_out$craps) {
    profit <- - pass_line + dont_pass
  } else if (!come_out$craps && is.na(come_out$point)) {
    profit <- pass_line - dont_pass
  } else if (!is.na(come_out$point)) {
    print(paste("Point established, point is:", come_out$point))
    roll <- resolve_point(come_out$point)
    
    if(sum(roll$rolls[nrow(roll$rolls), ]) == 7) {
      print("Seven out")
      profit <- - pass_line + dont_pass
    } else {
      print("Hit the point")
      profit <- pass_line - dont_pass
    }
  }
  
  return(list(
    roll = come_out$roll,
    profit = profit
  ))
}

roll_die <- function() {
  die = c(1:6)
  
  roll <- sample(die, 2, replace =TRUE)
  roll <- c(roll, sum(roll))
  names(roll) <- c("die1", "die2", "sum")
  
  return(roll)
}

come_out_roll <- function(pass_line_wager = 10, dont_pass_wager = 0) {
  roll <- roll_die()
  
  if (roll["sum"] == 7 || roll["sum"] == 11) {
    point <- NA
    profit <- pass_line_wager - dont_pass_wager
  } else if (roll["sum"] == 2 || roll["sum"] == 3) {
    point <- NA
    profit <- dont_pass_wager - pass_line_wager
  } else if (roll["sum"] == 12) {
    point <- NA
    profit <- -pass_line_wager
  } else {
    point <- roll[["sum"]]
    profit <- 0
  }
  
  return(list(
    roll = roll,
    point = point,
    profit = profit
  ))
}

resolve_point <- function(point) {
  roll <- roll_die()
  n_rolls <- 1
  rolls <- matrix(roll, nrow = 1)
  
  while (roll["sum"] != 7 && roll["sum"] != point) {
    roll <- roll_die()
    n_rolls <- n_rolls + 1
    rolls <- rbind(rolls, roll)
    colnames(rolls) <-  c("die1", "die2", "sum")
    rownames(rolls) <- NULL
  }
  
  return(list(
    rolls = rolls,
    n_rolls = n_rolls
  ))
}

play_craps <- function(pass_line_wager, dont_pass_wager) {
  come_out <- come_out_roll(pass_line_wager, dont_pass_wager)
  roll <- come_out$roll
  profit <- come_out$profit
  
  if (!is.na(come_out$point)) {
    resolve_point <- resolve_point(come_out$point)
    rolls <- resolve_point$rolls
    
    if (rolls[nrow(rolls), "sum"] == 7) {
      profit = dont_pass_wager - pass_line_wager
    } else {
      profit = pass_line_wager - dont_pass_wager
    }
  }
  
  return(list(
    roll = roll,
    rolls = rolls,
    profit = profit
  ))
}
