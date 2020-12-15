# --- Day 15: Rambunctious Recitation --- 
# You catch the airport shuttle and try
# to book a new flight to your vacation island. Due to the storm, all direct
# flights have been cancelled, but a route is available to get around the storm.
# You take it.
#
# While you wait for your flight, you decide to check in with the Elves back at
# the North Pole. They're playing a memory game and are ever so excited to
# explain the rules!
#
# In this game, the players take turns saying numbers. They begin by taking
# turns reading from a list of starting numbers (your puzzle input). Then, each
# turn consists of considering the most recently spoken number:
#
# If that was the first time the number has been spoken, the current player says
# 0. Otherwise, the number had been spoken before; the current player announces
# how many turns apart the number is from when it was previously spoken. So,
# after the starting numbers, each turn results in that player speaking aloud
# either 0 (if the last number is new) or an age (if the last number is a
# repeat).
#
# For example, suppose the starting numbers are 0,3,6:
# 
# Turn 1: The 1st number spoken is a starting number, 0.
# Turn 2: The 2nd number spoken is a starting number, 3.
# Turn 3: The 3rd number spoken is a starting number, 6.
# Turn 4: Now, consider the last number spoken, 6. Since that was the first time the number had been spoken, the 4th number spoken is 0.
# Turn 5: Next, again consider the last number spoken, 0. Since it had been spoken before, the next number to speak is the difference between the turn number when it was last spoken (the previous turn, 4) and the turn number of the time it was most recently spoken before then (turn 1). Thus, the 5th number spoken is 4 - 1, 3.
# Turn 6: The last number spoken, 3 had also been spoken before, most recently on turns 5 and 2. So, the 6th number spoken is 5 - 2, 3.
# Turn 7: Since 3 was just spoken twice in a row, and the last two turns are 1 turn apart, the 7th number spoken is 1.
# Turn 8: Since 1 is new, the 8th number spoken is 0.
# Turn 9: 0 was last spoken on turns 8 and 4, so the 9th number spoken is the difference between them, 4.
# Turn 10: 4 is new, so the 10th number spoken is 0.
# (The game ends when the Elves get sick of playing or dinner is ready, whichever comes first.)
# 
# Their question for you is: what will be the 2020th number spoken? In the example above, the 2020th number spoken will be 436.
# 
# Here are a few more examples:
# 
# Given the starting numbers 1,3,2, the 2020th number spoken is 1.
# Given the starting numbers 2,1,3, the 2020th number spoken is 10.
# Given the starting numbers 1,2,3, the 2020th number spoken is 27.
# Given the starting numbers 2,3,1, the 2020th number spoken is 78.
# Given the starting numbers 3,2,1, the 2020th number spoken is 438.
# Given the starting numbers 3,1,2, the 2020th number spoken is 1836.
# Given your starting numbers, what will be the 2020th number spoken?
# 
# Your puzzle input is 7,12,1,0,16,2.

library("data.table")

input_test <- c(0,3,6)

play_game <- function(start_nums, turns){
  turn_tracker <- data.frame("num" = start_nums, "turn" = 1:length(start_nums))
  turn <- max(turn_tracker$turn)
  while(max(turn_tracker$turn) < turns){
    num <- turn_tracker$num[turn_tracker$turn == turn]
    if(!num %in% turn_tracker$num[turn_tracker$turn < turn]){
      val <- 0
    } else {
      val <- tail(turn_tracker$turn[turn_tracker$num == num], 1) - tail(turn_tracker$turn[turn_tracker$num == num], 2)[1]
      
      if(length(tail(turn_tracker$turn[turn_tracker$num == num])) > 2){
        turn_tracker <- turn_tracker[-which(turn_tracker$num == num)[1],]
      }

    }
    turn_tracker <- turn_tracker %>% add_row("num" = val, "turn" =  turn + 1)
    
    turn <- turn + 1
  }
  
  return(turn_tracker %>% slice(n()) %>% pull(num))
}


play_game(input_test, 2020)
play_game(c(2,1,3), 2020)
play_game(c(1,2,3), 2020)
play_game(c(2,3,1), 2020)
play_game(c(3,2,1), 2020)
play_game(c(3,1,2), 2020)

play_game(c(7,12,1,0,16,2), 2020)

# -- Part Two ---
# Impressed, the Elves issue you a challenge: determine the 30000000th number
# spoken. For example, given the same starting numbers as above:
#   
# Given 0,3,6, the 30000000th number spoken is 175594.
# Given 1,3,2, the 30000000th number spoken is 2578.
# Given 2,1,3, the 30000000th number spoken is 3544142.
# Given 1,2,3, the 30000000th number spoken is 261214.
# Given 2,3,1, the 30000000th number spoken is 6895259.
# Given 3,2,1, the 30000000th number spoken is 18.
# Given 3,1,2, the 30000000th number spoken is 362.
# Given your starting numbers, what will be the 30000000th number spoken?

# play_game() is too slow to run -- it would likely take a couple days to brute force a solution. 
# Apparently even base data frames don't use key/value mapping: 
# https://stackoverflow.com/questions/49568627/dictionary-mapping-using-r

#We'll try with data.table, which should be faster because it does some key/value stuff, I think?: 
play_dt_game <- function(start_nums, turns){
  turn_tracker <- data.table("num" = start_nums, "turn" = 1:length(start_nums))
  turn <- max(turn_tracker$turn)

  for(i in turn:(turns-1)){
    num <- tail(turn_tracker$num, 1)
    val <- 0
    
    prev_turns <- turn_tracker$turn[turn_tracker$num == num]
    
    if(length(prev_turns) > 1){
      val <- diff(prev_turns)
      r <- which(turn_tracker$turn == prev_turns[1])
      turn_tracker <- turn_tracker[-r]
    }
    
    turn_tracker <- rbind(turn_tracker, 
                          data.table("num" = val, "turn" =  i + 1))
  }
  
  return(turn_tracker[turn_tracker$turn == turns, "num"])
}


system.time({play_game(input_test, 2020)})
system.time({play_dt_game(input_test, 2020)})

system.time({play_game(input_test, 10000)})
system.time({play_dt_game(input_test, 10000)})

#this is about 2x as fast, but it's still going to be way too slow for the whole thing :()

#Let's try with hash mapping:
play_hash_game <- function(start_nums, turns){
  tt <- list2env(
    as.list(
      setNames(1:(length(start_nums)-1), 
               head(start_nums,length(start_nums) - 1))
    )
  )
  
  turn <- length(start_nums)
  num <- start_nums[turn]

  for(turn in length(start_nums):turns){
    in_dict <- get0(as.character(num), tt)
    assign(as.character(num), turn, envir = tt)
    
    if(is.null(in_dict)){
      num <- 0
    } else {
      num <- turn - in_dict
    }
  }

  return(as.numeric(names(which(as.list(tt) == turns))))
}



play_hash_game(input_test, 2020)

system.time({play_game(input_test, 2020)})
system.time({play_dt_game(input_test, 2020)})
system.time({play_hash_game(input_test, 2020)})

#97% faster! Test on a bigger #: 
system.time(out <- play_hash_game(c(7,12,1,0,16,2), 100000)) 

# this still takes 36 seconds  -- it could probably finish in 3-5 hours, 
# but there has to be a better way. 
# 
# Checking against this solution: 
# https://github.com/czeildi/adventofcode2020/blob/main/solutions/day15.R

# This solution did not use data frames, data tables, or hashing, and returned the answer in about 30 seconds! 
# Instead of storing values at all, she set up a vector with NA for each turn, 
# and then stored the turn a number appeared at that position in the vector. 
# This means no looking up/searching -- just getting the number's position in the vector of indices
# will get you the last turn that number appeared. 

#Adapting this approach: 

play_vector_game <- function(start_nums, turns){
  tt <- c()
  for(i in 1:length(start_nums) - 1){
    tt[start_nums[i] + 1] <- i 
  }
  
  turn <- length(start_nums)
  num <- start_nums[turn]
  
  for(turn in length(start_nums):turns){
    #apparently vectors don't care whether the position exists or not, so we
    #don't need to pre-create a 30 million length vector, we can just add as we go
    in_dict <- tt[num + 1] 
    tt[num + 1] <- turn
    if(is.na(in_dict)){
      num <- 0
    } else {
      num <- turn - in_dict
    }
  }
  
  return(which(tt == turns) - 1)
}

play_vector_game(input_test, 2020)
system.time(out <- play_vector_game(c(7,12,1,0,16,2), 30000000)) 

# This actually winds up being faster - about 8 seconds! 
