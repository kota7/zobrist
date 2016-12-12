## examples

### tic tac toe

library(zobrist)
library(magrittr)
library(ggplot2)
library(tidyr)
library(reshape2)


## define the value function by a zobrist hash table of bitsize 20, where
##
##  1  ~ 9    ... indicates if the position is filled by X
## 11 ~ 19    ... indicates if the position is filled by O
## 10, 20     ... unused
##
## assume the indices are column-first, that is,
## index i corresponds to [(i-1) %% 3) + 1, (i+2) %/% 3]
## position (i, j) corresponds to 3*(j-1) + i

state_to_key <- function(state)
{
  # returns the key for Q-function corresponding to the state
  #
  # state    : 3x3 matrix of 0 (blank), 1 (X), and 2 (O)
  #
  # returns:
  #   integer vector of key
  c(which(state == 1L), which(state == 2L)+10)
}

index_to_coord <- function(i)
{
  c(((i-1) %% 3) + 1, (i+2) %/% 3)
}


legal_moves <- function(state)
{
  # returns all legal moves in the state
  # state    : 3x3 matrix of 0 (blank), 1 (X), and 2 (O)
  #
  # returns:
  #   integer vector of indices of legal moves

  which(state == 0L)
}


epsilon_greedy <- function(state, epsilon)
{
  # choose a move using the epsilon greedy
  #
  # values : vector of values
  # epsilon: probability of making a random choice
  #
  # returns:
  #   integer

  choices <- legal_moves(state)

  if (runif(1) < epsilon) {
    # random choice
    i <- sample.int(length(choices), 1)
    return(choices[i])
  }

  return(optim_action(state, conservative = FALSE))
}



optim_action <- function(state, conservative = TRUE)
{
  turn <- 2L - (sum(state == 0) %% 2)
  choices <- legal_moves(state)

  ## get the current value of each choice
  ## if the value is undefined, impute zero
  values <- rep(0, length(choices))
  for (i in seq_along(choices))
  {
    v <- Vfunc$get(state_to_key(new_state(state, choices[i], turn)))
    if (is.null(v)) {
      if (conservative) {
        # give a bit of penalty to undefined values
        if (turn == 1L) {
          v <- -5
        } else {
          v <- 5
        }
      } else {
        # try undefined values
        if (turn == 1L) {
          v <- 5
        } else {
          v <- -5
        }
      }
    }
    values[i] <- v
  }

  if (turn == 2L) values <- -values
  index <- which(values == max(values))
  i <- index[sample.int(length(index), 1)]
  choices[i]
}



check_win <- function(state, player)
{
  # retrun TRUE if the player gets a line in the state
  for (i in 1:3)
  {
    if (all(state[i,] == player)) return(TRUE)
    if (all(state[,i] == player)) return(TRUE)
  }
  if (all(state[cbind(1:3,1:3)] == player)) return(TRUE)
  if (all(state[cbind(1:3,3:1)] == player)) return(TRUE)

  return(FALSE)
}


new_state <- function(state, move, turn)
{
  coord <- index_to_coord(move)
  state[coord[1], coord[2]] <- turn
  state
}

show_state <- function(state)
{
  o <- matrix("+", nrow = 3, ncol = 3)
  o[state == 1L] <- "X"
  o[state == 2L] <- "O"

  for (i in 1:3) cat("\n  ", paste(o[i,], collapse = " "))
  cat("\n")
}

equivalent_states <- function(state)
{
  # returns all equivalent states

  s1 <- state[3:1, 1:3]   # vertical flip
  s2 <- state[1:3, 3:1]   # horizontal flip
  s3 <- state[3:1, 3:1]
  s4 <- t(state)          # transpose

  s5 <- s4[3:1, 1:3]
  s6 <- s4[1:3, 3:1]
  s7 <- s4[3:1, 3:1]
  list(state, s1, s2, s3, s4, s5, s6, s7)
}



### learning ###
train <- function(Vfunc, epsilon = 0.2, alpha = 0.9, gamma = 0.99, quiet = TRUE)
{
  # initialization
  turn <- 1L
  state <- matrix(0L, nrow = 3, ncol = 3)


  for (movenumber in 1:10)
  {
    if (!quiet) {
      cat("\n=======================\n")
      cat("Move:", movenumber, "\n")
    }
    ## if the current state is winninf for the other player, update the value
    ## and finish
    win <- check_win(state, 3L-turn)
    if (win) {
      # win = true if the other player wins
      if (!quiet) cat("player", 3-turn, "wins!\n")
      # if player 1 wins, positive, otherwise negative
      value <- 100 * (3 - 2*turn) * (-1)

      # update all equivalent states too
      for (s in unique(equivalent_states(state)))
        Vfunc$update(state_to_key(s), value)

      # this episode ends
      if (!quiet) cat("final state\n")
      if (!quiet) show_state(state)
      break
    }

    ## if the move number exceeds 9, the game is over
    ## give the state the value of 0
    if (movenumber > 9L) {
      if (!quiet) cat("draw!\n")
      # update all equivalent states too
      for (s in unique(equivalent_states(state)))
        Vfunc$update(state_to_key(s), 0)

      # this episode ends
      if (!quiet) cat("final state\n")
      if (!quiet) show_state(state)
      break
    }


    ## get the current value of each choice
    ## if the value is undefined, impute zero
    choices <- legal_moves(state)
    cur_values <- rep(0, length(choices))
    for (i in seq_along(choices))
    {
      v <- Vfunc$get(state_to_key(new_state(state, choices[i], turn)))
      if (!is.null(v)) cur_values[i] <- unlist(v)
    }

    # update value
    oldval <- Vfunc$get(state_to_key(state))
    if (is.null(oldval)) oldval <- 0
    if (turn == 1L) {
      newval <- (1-alpha)*oldval + alpha*gamma*max(cur_values)
    } else {
      newval <- (1-alpha)*oldval + alpha*gamma*min(cur_values)
    }
    keys <- lapply(unique(equivalent_states(state)), state_to_key)
    lapply(keys, Vfunc$update, newval)


    # player 1 maximizes value, while player 2 minimizes
    action <- epsilon_greedy(state, epsilon)

    if (!quiet) {
      cat("current state\n")
      show_state(state)
      cat("legal action\n")
      cat(choices, "\n")
      cat("action chosen\n")
      cat(action, "\n")
    }

    # update state
    state <- new_state(state, action, turn)

    # switch turn
    turn <- 3L - turn
  }
}









simulate <- function(times = 1000)
{
  # simulate game between the program
  results <- rep(NA, times)
  for (count in 1:times)
  {
    cat(sprintf("\rSimulating ...%4d/%4d", count, times))
    turn <- 1L
    state <- matrix(0L, nrow = 3, ncol = 3)

    for (mv in 1:9)
    {
      action <- optim_action(state)
      state <- new_state(state, action, turn)
      win <- check_win(state, turn)
      if (win) {
        winner <- turn
        break
      } else if (mv == 9) {
        winner <- 0L
        break
      }
      turn <- 3L - turn
    }
    results[count] <- winner
  }
  cat("\n")
  return(results)
}







Vfunc <- zobristht(20, 10, rehashable = TRUE)

## save low level AI
ttt_value0 <- Vfunc$clone()
devtools::use_data(ttt_value0, overwrite = TRUE)
rm(ttt_value0)


res <- simulate()
sim_res <- data.frame(prop.table(table(res)))
sim_res$simulation <- 0
max_train <- 5000
for (i in 1:max_train)
{
  cat(sprintf("\rTraining ...%4d/%4d", i, max_train))
  train(Vfunc, epsilon = 0.2)

  if ((i %% 100) == 0) {
    cat("\ntraining count:", i, "\n")
    res <- simulate()
    prop.table(table(res)) %>% print()
    tmp <- data.frame(prop.table(table(res)))
    tmp$simulation <- i
    sim_res <- rbind(sim_res, tmp)
  }


  if (i == 1000) {
    ttt_value1 <- Vfunc$clone()
    devtools::use_data(ttt_value1, overwrite = TRUE)
    rm(ttt_value1)
  }


  if (i == 1500) {
    ttt_value2 <- Vfunc$clone()
    devtools::use_data(ttt_value2, overwrite = TRUE)
    rm(ttt_value2)
  }

  if (i == 5000) {
    ttt_value3 <- Vfunc$clone()
    devtools::use_data(ttt_value3, overwrite = TRUE)
    rm(ttt_value3)
  }

}


# graphing
dat <- spread(sim_res, key = res, value = Freq, fill = 0) %>%
  melt(id.vars = "simulation", variable.name = "result_num", value.name = "frac")

dat$result <- ""
dat$result[dat$result_num == "0"] <- "Draw"
dat$result[dat$result_num == "1"] <- "Player1"
dat$result[dat$result_num == "2"] <- "Player2"
ggplot(dat, aes(simulation, frac, linetype = result, shape = result)) +
  geom_line(size = 1, color = "grey25") +
  geom_point(size = 2, color = "grey50") +
  geom_point(size = 1.5, color = "grey10") +
  xlab("number of training") + ylab("fraction") +
  theme_bw()
ggsave("example/output/ttt-learning-curve.pdf", width = 10, height = 6)
ggsave("example/output/ttt-learning-curve.png", width = 10, height = 6)
