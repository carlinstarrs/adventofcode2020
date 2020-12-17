# --- Day 17: Conway Cubes ---
# As your flight slowly drifts through the sky, the Elves at the Mythical
# Information Bureau at the North Pole contact you. They'd like some help
# debugging a malfunctioning experimental energy source aboard one of their
# super-secret imaging satellites.
#
# The experimental energy source is based on cutting-edge technology: a set of
# Conway Cubes contained in a pocket dimension! When you hear it's having
# problems, you can't help but agree to take a look.
#
# The pocket dimension contains an infinite 3-dimensional grid. At every integer
# 3-dimensional coordinate (x,y,z), there exists a single cube which is either
# active or inactive.
#
# In the initial state of the pocket dimension, almost all cubes start inactive.
# The only exception to this is a small flat region of cubes (your puzzle
# input); the cubes in this region start in the specified active (#) or inactive
# (.) state.
#
# The energy source then proceeds to boot up by executing six cycles.
#
# Each cube only ever considers its neighbors: any of the 26 other cubes where
# any of their coordinates differ by at most 1. For example, given the cube at
# x=1,y=2,z=3, its neighbors include the cube at x=2,y=2,z=2, the cube at
# x=0,y=2,z=3, and so on.
#
# During a cycle, all cubes simultaneously change their state according to the
# following rules:
#
# If a cube is active and exactly 2 or 3 of its neighbors are also active, the
# cube remains active. Otherwise, the cube becomes inactive. If a cube is
# inactive but exactly 3 of its neighbors are active, the cube becomes active.
# Otherwise, the cube remains inactive. The engineers responsible for this
# experimental energy source would like you to simulate the pocket dimension and
# determine what the configuration of cubes should be at the end of the
# six-cycle boot process.
#
# For example, consider the following initial state:
# 
# .#.
# ..#
# ###
# Even though the pocket dimension is 3-dimensional, this initial state
# represents a small 2-dimensional slice of it. (In particular, this initial
# state defines a 3x3x1 region of the 3-dimensional space.)
#
# Simulating a few cycles from this initial state produces the following
# configurations, where the result of each cycle is shown layer-by-layer at each
# given z coordinate:
#
# Before any cycles:
# 
# z=0
# .#.
# ..#
# ###
#
# ...
#
# After the full six-cycle boot process completes, 112 cubes are left in the
# active state.
#
# Starting with your given initial configuration, simulate six cycles. How many
# cubes are left in the active state after the sixth cycle?
library("tidyverse")

input_test <- c(".#.",
                "..#",
                "###")

input <- readLines("C:/Users/Carlin/Documents/GitHub/adventofcode2020/day17/input.txt")

parse_input <- function(dat){
  data.frame("V1" = dat) %>%  
    separate(col = V1, 
             sep = "", 
             into = paste0("V", 1:(nchar(dat[1]) + 1))) %>% 
    select(-V1) %>% 
    mutate(x = 1:n()) %>% 
    pivot_longer(cols = matches("V."), 
                 names_to = "y") %>% 
    mutate(y = as.numeric(gsub("V", "", y)) - 1, 
           z = 0) %>% 
    select(x, y, z, value)
}



dat <- parse_input(input_test)
dat <- parse_input(input)
cycles <- 6

cycle <- 1
while(cycle <= cycles){
  print(paste0("cycle: ", cycle))
  next_grid <- function(dat){
    xdim <- min(dat$x - 1):max(dat$x + 1)
    ydim <- min(dat$y - 1):max(dat$y + 1)
    zdim <- min(dat$z - 1):max(dat$z + 1)
    
    expand.grid(x = xdim, y= ydim, z = zdim)
  }
  
  ng <- next_grid(dat) %>% 
    left_join(dat, by = c("x", "y", "z")) %>%
    mutate(value = replace_na(value, "."))
  
  new_state <- function(dat, ng, x, y, z, ...){
    cube <- ng[ng$x == x & ng$y == y & ng$z == z,]
    vals <- map(c(x,y,z), ~c((.x - 1):(.x + 1)))
    
    cube_state <- ng$value[ng$x == x & ng$y == y & ng$z == z]
    
    if(!identical(cube_state, character(0))){
      nbs <- ng %>%
        filter(x %in% vals[[1]] & y %in% vals[[2]] & z %in% vals[[3]]) %>% 
        anti_join(cube, by = c("x", "y", "z"))
      
      active_neighbors <- sum(nbs$value == "#", na.rm = TRUE)
      
      if(cube_state == "#" & !active_neighbors %in% c(2,3)){
        cube_state <- "."
      } else if (cube_state == "." & active_neighbors == 3){
        cube_state <- "#"
      }
      
      return(cube %>% mutate(value = cube_state))
    }
  }
  
  dat <- pmap_dfr(ng, function(...){
    exec(new_state, dat = dat, ng = ng, !!!list(...))
  })
  
  cycle <- cycle + 1
}

sum(dat$value == "#")

# a <- out %>% filter(z == 0)
# matrix(out %>% filter(z == 0) %>% pull(value), ncol = 5, nrow = 5)

# --- Part Two --- 
# For some reason, your simulated results don't match what the
# experimental energy source engineers expected. Apparently, the pocket
# dimension actually has four spatial dimensions, not three.
#
# The pocket dimension contains an infinite 4-dimensional grid. At every integer
# 4-dimensional coordinate (x,y,z,w), there exists a single cube (really, a
# hypercube) which is still either active or inactive.
#
# Each cube only ever considers its neighbors: any of the 80 other cubes where
# any of their coordinates differ by at most 1. For example, given the cube at
# x=1,y=2,z=3,w=4, its neighbors include the cube at x=2,y=2,z=3,w=3, the cube
# at x=0,y=2,z=3,w=4, and so on.
#
# The initial state of the pocket dimension still consists of a small flat
# region of cubes. Furthermore, the same rules for cycle updating still apply:
# during each cycle, consider the number of active neighbors of each cube.
#
# For example, consider the same initial state as in the example above. Even
# though the pocket dimension is 4-dimensional, this initial state represents a
# small 2-dimensional slice of it. (In particular, this initial state defines a
# 3x3x1x1 region of the 4-dimensional space.)
#
# Simulating a few cycles from this initial state produces the following
# configurations, where the result of each cycle is shown layer-by-layer at each
# given z and w coordinate:
#
# ...
#
# After the full six-cycle boot process completes, 848 cubes are left in the
# active state.
#
# Starting with your given initial configuration, simulate six cycles in a
# 4-dimensional space. How many cubes are left in the active state after the
# sixth cycle?


input_test <- c(".#.",
                "..#",
                "###")

input <- readLines("C:/Users/Carlin/Documents/GitHub/adventofcode2020/day17/input.txt")

parse_input <- function(dat){
  data.frame("V1" = dat) %>%  
    separate(col = V1, 
             sep = "", 
             into = paste0("V", 1:(nchar(dat[1]) + 1))) %>% 
    select(-V1) %>% 
    mutate(x = 1:n()) %>% 
    pivot_longer(cols = matches("V."), 
                 names_to = "y") %>% 
    mutate(y = as.numeric(gsub("V", "", y)) - 1, 
           z = 0, 
           w = 0) %>% 
    select(x, y, z, w, value)
}



dat <- parse_input(input_test)
dat <- parse_input(input)
cycles <- 6

cycle <- 1
while(cycle <= cycles){
  print(paste0("cycle: ", cycle))
  next_grid <- function(dat){
    xdim <- min(dat$x - 1):max(dat$x + 1)
    ydim <- min(dat$y - 1):max(dat$y + 1)
    zdim <- min(dat$z - 1):max(dat$z + 1)
    wdim <- min(dat$w - 1):max(dat$w + 1)
    
    expand.grid(x = xdim, y= ydim, z = zdim, w = wdim)
  }
  
  ng <- next_grid(dat) %>% 
    left_join(dat, by = c("x", "y", "z", "w")) %>%
    mutate(value = replace_na(value, "."))
  
  new_state <- function(dat, ng, x, y, z, w, ...){
    cube <- ng[ng$x == x & ng$y == y & ng$z == z & ng$w == w,]
    cube_state <- ng$value[ng$x == x & ng$y == y & ng$z == z & ng$w == w]
    
    if(!identical(cube_state, character(0))){
      vals <- map(c(x,y,z,w), ~c((.x - 1):(.x + 1)))

      nbs <- ng %>%
        filter(x %in% vals[[1]] & y %in% vals[[2]] & z %in% vals[[3]] & w %in% vals[[4]]) %>% 
        anti_join(cube, by = c("x", "y", "z", "w"))
      
      active_neighbors <- sum(nbs$value == "#", na.rm = TRUE)
      
      if(cube_state == "#" & !active_neighbors %in% c(2,3)){
        cube_state <- "."
      } else if (cube_state == "." & active_neighbors == 3){
        cube_state <- "#"
      }
      
      return(cube %>% mutate(value = cube_state))
    }
  }
  
  dat <- pmap_dfr(ng, function(...){
    exec(new_state, dat = dat, ng = ng, !!!list(...))
  })
  
  cycle <- cycle + 1
}

sum(dat$value == "#")


dat %>% filter(z == 0 & w == 0) %>% pull(value) %>% matrix(ncol = 9, nrow = 9)
