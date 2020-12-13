# --- Day 12: Rain Risk ---
# Your ferry made decent progress toward the island, but the storm came in
# faster than anyone expected. The ferry needs to take evasive actions!
#
# Unfortunately, the ship's navigation computer seems to be malfunctioning;
# rather than giving a route directly to safety, it produced extremely
# circuitous instructions. When the captain uses the PA system to ask if anyone
# can help, you quickly volunteer.
#
# The navigation instructions (your puzzle input) consists of a sequence of
# single-character actions paired with integer input values. After staring at
# them for a few minutes, you work out what they probably mean:
# 
# Action N means to move north by the given value.
# Action S means to move south by the given value.
# Action E means to move east by the given value.
# Action W means to move west by the given value.
# Action L means to turn left the given number of degrees.
# Action R means to turn right the given number of degrees.
# Action F means to move forward by the given value in the direction the ship is currently facing.
#
# The ship starts by facing east. 
# Only the L and R actions change the direction the ship is facing. 
# (That is, if the ship is facing east and the next instruction is N10, the ship
# would move north 10 units, but would still move east if the following action
# were F.)
# 
# For example:
# 
# F10
# N3
# F7
# R90
# F11
#
# These instructions would be handled as follows:
# 
# F10 would move the ship 10 units east (because the ship starts by facing east) to east 10, north 0.
# N3 would move the ship 3 units north to east 10, north 3.
# F7 would move the ship another 7 units east (because the ship is still facing east) to east 17, north 3. 
# R90 would cause the ship to turn right by 90 degrees and face south; it remains at east 17, north 3. 
# F11 would move the ship 11 units south to east 17, south 8. 

# At the end of these instructions, the
# ship's Manhattan distance (sum of the absolute values of its east/west
# position and its north/south position) from its starting position is 17 + 8 =
# 25.
#
# Figure out where the navigation instructions lead. What is the Manhattan
# distance between that location and the ship's starting position?
library("tidyverse")
input_test <- c("F10", "N3", "F7", "R90", "F11")
input <- readLines("C:/Users/Carlin/Documents/GitHub/adventofcode2020/day12/input.txt")


interpret <- function(b, bearing, movedir = movedir){
  inst <- str_extract(b, "^.")
  if(inst %in% c("N", "S", "E", "W", "F")){
    type <- "movement"
    units <- str_sub(b, 2, nchar(b))
    if(inst != "F"){
      type <- c(type, "rotation")
      movedir <- inst
    } else {
      movedir <-  switch(as.character(bearing), 
                         "0" = "N", 
                         "90" = "E", 
                         "180" = "S", 
                         "270" = "W")
    }
  } else if (inst %in% c("L", "R")){
    type <- "rotation"
    degrees <- as.numeric(str_sub(b, 2, nchar(b)))
    bearing <- switch(inst, 
                      "R" = bearing + degrees, 
                      "L" = bearing - degrees)
    units <- 0
  } 
  
  bearing[bearing < 0] <- 360 + bearing[bearing < 0]
  bearing[bearing >= 360] <- bearing[bearing >= 360]  - 360
  
  return(list("bearing" = as.numeric(bearing), 
              "units" = as.numeric(units), 
              "movedir" = as.character(movedir)))
}


move_the_ship <- function(dat, start_bearing){
  movement <- tibble(inst = "start", 
                     bearing = 90, 
                     units = 0, 
                     movedir = switch(as.character(bearing), 
                                      "0" = "N", 
                                      "90" = "E", 
                                      "180" = "S", 
                                      "270" = "W"))
  
  for(i in 1:length(dat)){
    instruction <- interpret(dat[i], movement$bearing[nrow(movement)], movement$movedir[nrow(movement)])
    
    movement <- movement %>% add_row(inst = dat[i], 
                                     bearing = instruction$bearing, 
                                     units = instruction$units, 
                                     movedir = instruction$movedir)
  }
  
  return(movement)
}

movement_summary <- function(movement){
  movement %>% 
    mutate(units = case_when(movedir == "W" ~ -1 * as.numeric(units), 
                             movedir == "N" ~ -1 * as.numeric(units), 
                             TRUE ~ units), 
           plane = case_when(movedir %in% c("N", "S") ~ "y", 
                             movedir %in% c("E", "W") ~ "x")) %>% 
    group_by(plane) %>% 
    mutate(cum_units = cumsum(units)) %>% 
    slice(n()) %>% 
    mutate(cum_units = abs(cum_units)) %>% 
    pull(cum_units) %>% 
    sum()
  
}

test <- move_the_ship(input_test, 90) %>% movement_summary()

out <- move_the_ship(input, 90) %>% movement_summary() 

# --- Part Two ---
# Before you can give the destination to the captain, you realize that the
# actual action meanings were printed on the back of the instructions the whole
# time.
# 
# Almost all of the actions indicate how to move a waypoint which is relative to
# the ship's position:
# 
# Action N means to move the waypoint north by the given value.
# Action S means to move the waypoint south by the given value.
# Action E means to move the waypoint east by the given value.
# Action W means to move the waypoint west by the given value.
# Action L means to rotate the waypoint around the ship left (counter-clockwise) the given number of degrees.
# Action R means to rotate the waypoint around the ship right (clockwise) the given number of degrees.
# Action F means to move forward to the waypoint a number of times equal to the given value.
#
# The waypoint starts 10 units east and 1 unit north relative to the ship. The
# waypoint is relative to the ship; that is, if the ship moves, the waypoint
# moves with it.
# 
# For example, using the same instructions as above:
# 
# F10 moves the ship to the waypoint 10 times (a total of 100 units east and 10 units north), leaving the ship at east 100, north 10. The waypoint stays 10 units east and 1 unit north of the ship.
# N3 moves the waypoint 3 units north to 10 units east and 4 units north of the ship. The ship remains at east 100, north 10.
# F7 moves the ship to the waypoint 7 times (a total of 70 units east and 28 units north), leaving the ship at east 170, north 38. The waypoint stays 10 units east and 4 units north of the ship.
# R90 rotates the waypoint around the ship clockwise 90 degrees, moving it to 4 units east and 10 units south of the ship. The ship remains at east 170, north 38.
# F11 moves the ship to the waypoint 11 times (a total of 44 units east and 110 units south), leaving the ship at east 214, south 72. The waypoint stays 4 units east and 10 units south of the ship.
# After these operations, the ship's Manhattan distance from its starting position is 214 + 72 = 286.
# 
# Figure out where the navigation instructions actually lead. What is the Manhattan distance 
# between that location and the ship's starting position?

turn_waypoint <- function(b, x, y, bearing){
  inst <- str_extract(b, "^.")
  degrees <- as.numeric(str_sub(b, 2, nchar(b)))
  
  if(inst == "L"){
    newxy <- switch(as.character(abs(degrees)), 
                    "0" = c(x, y),
                    "90" = c(-y, x),
                    "180" = c(-x, -y), 
                    "270" = c(y, -x), 
                    "360" = c(x, y))
  } else {
      newxy <- switch(as.character(abs(degrees)), 
                      "0" = c(x, y),
                      "90" = c(y, -x),
                      "180" = c(-x, -y), 
                      "270" = c(-y, x), 
                      "360" = c(x, y))
  }
  new_bearing <- switch(inst,
                        "R" = bearing + degrees,
                        "L" = bearing - degrees)

  new_bearing[new_bearing < 0] <- 360 + new_bearing[new_bearing < 0]
  new_bearing[new_bearing >= 360] <- new_bearing[new_bearing >= 360]  - 360

  return(list("x" = newxy[1], 
              "y" = newxy[2], 
              "bearing" = new_bearing))
}

move_the_ship2 <- function(b, ship_x, ship_y, waypoint_x, waypoint_y, bearing){
  inst <- str_extract(b, "^.")
  units <- as.numeric(str_sub(b, 2, nchar(b)))

  shift <- c(waypoint_x*units, waypoint_y*units)
  
  return(list("x" = ship_x + shift[1], 
              "y" = ship_y + shift[2]))
}


move_the_waypoint <- function(b, x, y){
  inst <- str_extract(b, "^.")
  units <- as.numeric(str_sub(b, 2, nchar(b)))
  newxy <- switch(inst, 
                  "N" = c(x, y + units), 
                  "E" = c(x + units, y), 
                  "S" = c(x, y - units), 
                  "W" = c(x - units, y))
  
  return(list("x" = newxy[1], 
              "y" = newxy[2]))
}

movement <- tibble(inst = "start", 
                   bearing = 90,
                   waypoint_x = 10, 
                   waypoint_y = 1, 
                   ship_x = 0, 
                   ship_y = 0)


lets_go <- function(dat, ship, waypoint, bearing){
  movement <- tibble(inst = "start", 
                     bearing = bearing,
                     waypoint_x = waypoint[1], 
                     waypoint_y = waypoint[2], 
                     ship_x = ship[1], 
                     ship_y = ship[2])
  
  for(i in 1:length(dat)){
    if(grepl("L|R", dat[i])){
      moves <- turn_waypoint(b = dat[i], 
                             x = movement$waypoint_x[nrow(movement)], 
                             y = movement$waypoint_y[nrow(movement)],
                             bearing = movement$bearing[nrow(movement)])
      rowadd <- tibble(inst = dat[i], 
                       bearing = moves$bearing, 
                       waypoint_x = moves$x, 
                       waypoint_y = moves$y, 
                       ship_x = movement$ship_x[nrow(movement)], 
                       ship_y = movement$ship_y[nrow(movement)])
      
    } else if(grepl("F", dat[i])){
      moves <- move_the_ship2(b = dat[i], 
                              ship_x = movement$ship_x[nrow(movement)], 
                              ship_y = movement$ship_y[nrow(movement)],
                              waypoint_x = movement$waypoint_x[nrow(movement)], 
                              waypoint_y = movement$waypoint_y[nrow(movement)], 
                              bearing = movement$bearing[nrow(movement)])
      rowadd <- tibble(inst = dat[i], 
                       bearing = movement$bearing[nrow(movement)], 
                       waypoint_x = movement$waypoint_x[nrow(movement)], 
                       waypoint_y = movement$waypoint_y[nrow(movement)], 
                       ship_x = moves$x, 
                       ship_y = moves$y)
      
    } else {
      moves <- move_the_waypoint(b = dat[i], 
                                 x = movement$waypoint_x[nrow(movement)], 
                                 y = movement$waypoint_y[nrow(movement)])
      
      rowadd <- tibble(inst = dat[i], 
                       bearing = movement$bearing[nrow(movement)], 
                       waypoint_x = moves$x, 
                       waypoint_y = moves$y, 
                       ship_x = movement$ship_x[nrow(movement)], 
                       ship_y = movement$ship_y[nrow(movement)])
    }

    movement <- movement %>% 
      bind_rows(rowadd)
  }
  return(movement)
}



lets_go(dat = input_test, 
        ship = c(0,0),
        waypoint = c(10, 1), 
        bearing = 90)

out <- lets_go(dat = input, 
               ship = c(0,0),
               waypoint = c(10, 1), 
               bearing = 90) 

out %>% 
  slice(n()) %>% 
  mutate(SUM = sum(abs(ship_x), abs(ship_y))) %>% 
  pull(SUM)

