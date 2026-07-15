roll_die <- function() {
  die <- c(1:6)
  roll <- sample(die, 2, replace = TRUE)
  
  return(roll)
}

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
  rolls <- roll
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
