# Misc setup --------------------------------------------------------------
# required packages
pkgs <- c("dplyr", "stringr", "tidyr", "reshape")
# install packages if required
installed_packages <- pkgs %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
 install.packages(pkgs[!installed_packages])
}
# load required packages:
invisible(lapply(pkgs, library, character.only = TRUE))
# load data
v_rps_games <- read.csv(file = "data-raw/02_day/02_data.txt", 
                        sep = "\\", header = FALSE)

# Part 1 ------------------------------------------------------------------
## Data wrangling ----------------------------------------------------------
# split data into columns
v_rps_games <- colsplit(x = v_rps_games[, 1], split = "", 
                        names = c("Player_1", "Player_2"))
v_rps_games <- v_rps_games[, c(1, 3)]
colnames(v_rps_games) <- c("Player_1", "Player_2")
# code as factor
for (i in 1:ncol(v_rps_games)) {
 v_rps_games[, i] <- as.factor(v_rps_games[, i])
}
# recode variables to be easily readable values
v_rps_games[, 1] <- recode(v_rps_games[, 1], "A" = "rock", "B" = "paper", "C" = "scissors")
v_rps_games[, 2] <- recode(v_rps_games[, 2], "X" = "rock", "Y" = "paper", "Z" = "scissors")
# add two columns to store result and total score values
v_rps_games_v2 <- cbind(v_rps_games, "Strategy_Score" = NA, "Result" = NA, "Total" = NA)

## Calculations ------------------------------------------------------------
# iterate through games to find solution
for (i in 1:nrow(v_rps_games_v2)) {
 # iterate over strategy score for player 2: 1 for rock, 2 for paper, 3 for scissors
 if (v_rps_games_v2[i, "Player_2"] == "rock") {
  v_rps_games_v2[i, "Strategy_Score"] <- 1
 } else if (v_rps_games_v2[i, "Player_2"] == "paper") {
  v_rps_games_v2[i, "Strategy_Score"] <- 2
  } else {
   v_rps_games_v2[i, "Strategy_Score"] <- 3
   }
 # iterate and calculate each game's result
 if (v_rps_games_v2[i, "Player_1"] == "rock" && v_rps_games_v2[i, "Player_2"] == "scissors" | 
     v_rps_games_v2[i, "Player_1"] == "paper" && v_rps_games_v2[i, "Player_2"] == "rock" | 
     v_rps_games_v2[i, "Player_1"] == "scissors" && v_rps_games_v2[i, "Player_2"] == "paper") {
  v_rps_games_v2[i, "Result"] <- 0
  } else if (v_rps_games_v2[i, "Player_2"] == "rock" && v_rps_games_v2[i, "Player_1"] == "scissors" | 
             v_rps_games_v2[i, "Player_2"] == "paper" && v_rps_games_v2[i, "Player_1"] == "rock" | 
             v_rps_games_v2[i, "Player_2"] == "scissors" && v_rps_games_v2[i, "Player_1"] == "paper") {
   v_rps_games_v2[i, "Result"] <- 6
   } else if (v_rps_games_v2[i, "Player_1"] == v_rps_games_v2[i, "Player_2"]) {
    v_rps_games_v2[i, "Result"] <- 3
    }
 }
# calculate sum score
for (i in 1:nrow(v_rps_games_v2)) {
 v_rps_games_v2[i, "Total"] <- v_rps_games_v2[i, "Strategy_Score"] + v_rps_games_v2[i, "Result"]
}
# print total
sum(v_rps_games_v2$Total)

# Part 2 ------------------------------------------------------------------
## Data Wrangling ----------------------------------------------------------
v_rps_games_v3 <- read.csv(file = "data-raw/02_day/02_data.txt", sep = "\\", header = FALSE)
# Rules:
# if Y (draw), Player 2 == Player 1
# if X (lose), Player 2 < Player 1
# if Z (win), Player 2 > Player 1
v_rps_games_v3 <- colsplit(x = v_rps_games_v3[, 1], split = "", 
                        names = c("Player_1", "Strategy"))
v_rps_games_v3 <- v_rps_games_v3[, c(1, 3)]
colnames(v_rps_games_v3) <- c("Player_1", "Strategy")
# code as factor
for (i in 1:ncol(v_rps_games_v3)) {
 v_rps_games_v3[, i] <- as.factor(v_rps_games_v3[, i])
}
# recode variables to be easily readable values
v_rps_games_v3[, 1] <- recode(v_rps_games_v3[, 1], "A" = "rock", "B" = "paper", "C" = "scissors")
v_rps_games_v3[, 2] <- recode(v_rps_games_v3[, 2], "X" = "lose", "Y" = "draw", "Z" = "win")
# add two columns to store result and total score values
v_rps_games_v4 <- cbind(v_rps_games_v3, "Player_2" = NA, "Strategy_Score" = NA, 
                        "Result" = NA)

## Calculations ------------------------------------------------------------
# iterate through games to find solution
for (i in 1:nrow(v_rps_games_v4)) {
 # if draw strategy then match player 1
 if (v_rps_games_v4[i, "Strategy"] == "draw") {
  v_rps_games_v4[i, "Player_2"] <- as.character(v_rps_games_v4[i, "Player_1"])
  # if lose strategy and player 1 = rock, choose scissors
  } else if (v_rps_games_v4[i, "Strategy"] == "lose" &
             v_rps_games_v4[i, "Player_1"] == "rock") {
   v_rps_games_v4[i, "Player_2"] <- "scissors"
  # if lose strategy and player 1 = paper, choose rock
   } else if (v_rps_games_v4[i, "Strategy"] == "lose" &
              v_rps_games_v4[i, "Player_1"] == "paper") {
    v_rps_games_v4[i, "Player_2"] <- "rock"
    # if lose strategy and player 1 = scissors, choose paper
    } else if (v_rps_games_v4[i, "Strategy"] == "lose" &
               v_rps_games_v4[i, "Player_1"] == "scissors") {
     v_rps_games_v4[i, "Player_2"] <- "paper"
     # if win strategy and player 1 = rock, choose paper
     } else if (v_rps_games_v4[i, "Strategy"] == "win" &
                v_rps_games_v4[i, "Player_1"] == "rock") {
      v_rps_games_v4[i, "Player_2"] <- "paper"
      # if win strategy and player 1 = paper, choose scissors
      } else if (v_rps_games_v4[i, "Strategy"] == "win" &
                 v_rps_games_v4[i, "Player_1"] == "paper") {
       v_rps_games_v4[i, "Player_2"] <- "scissors"
       # if win strategy and player 1 = scissors, choose rock
       } else if (v_rps_games_v4[i, "Strategy"] == "win" &
                  v_rps_games_v4[i, "Player_1"] == "scissors") {
        v_rps_games_v4[i, "Player_2"] <- "rock"
       }
 # iterate over strategy score for player 2: 1 for rock, 2 for paper, 3 for scissors
 if (v_rps_games_v4[i, "Player_2"] == "rock") {
  v_rps_games_v4[i, "Strategy_Score"] <- 1
  } else if (v_rps_games_v4[i, "Player_2"] == "paper") {
   v_rps_games_v4[i, "Strategy_Score"] <- 2
   } else {
    v_rps_games_v4[i, "Strategy_Score"] <- 3
    }
 # iterate and calculate total
 if (v_rps_games_v4[i, "Strategy"] == "lose") {
  v_rps_games_v4[i, "Result"] <- v_rps_games_v4[i, "Strategy_Score"] + 0
  } else if (v_rps_games_v4[i, "Strategy"] == "win") {
   v_rps_games_v4[i, "Result"] <- v_rps_games_v4[i, "Strategy_Score"] + 6
   } else if (v_rps_games_v4[i, "Strategy"] == "draw") {
    v_rps_games_v4[i, "Result"] <- v_rps_games_v4[i, "Strategy_Score"] + 3
   }
}
# print total
sum(v_rps_games_v4$Result)

# End file ----------------------------------------------------------------