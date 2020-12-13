# All decisions are based on the number of occupied seats adjacent to a given
# seat (one of the eight positions immediately up, down, left, right, or
# diagonal from the seat). The following rules are applied to every seat
# simultaneously:
#
# If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied. 
# If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty. 
# Otherwise, the seat's state does not change.
# Floor (.) never changes; seats don't move, and nobody sits on the floor.
#
# After one round of these rules, every seat in the example layout becomes
# occupied:
# 

library("tidyverse")
input_test <- read.csv("C:/Users/Carlin/Documents/GitHub/adventofcode2020/day11/input_test.txt", header = FALSE) 
input <- read.csv("C:/Users/Carlin/Documents/GitHub/adventofcode2020/day11/input.txt", header = FALSE) 

parse_input <- function(dat){
  dat %>% 
    pmap_dfr(function(...) {
      x <- tibble(...)
      x <- unlist(strsplit(as.character(x), ""))
      names(x) <- paste0("col", 1:length(x))
      return(x)
    })
}

####PART 1####
seat_mapper <- function(dat, x, y){
  seat <- dat %>% filter(rowid == x) %>% filter(colid == y)
  if(seat$value != "."){
    adj_seats <- dat %>% filter(rowid %in% (x+1):(x-1) & colid %in% (y-1):(y+1)) %>% anti_join(seat, by = c("rowid", "colid", "value"))
    
    if(!any(adj_seats$value == "#")){ #if seats around are empty, occupy
      new_val <- "#"
    } else if (sum(adj_seats$value == "#") >= 4){ #if >=4 adjacent seats are occupied, empty
      new_val  <- "L"
    } else { #otherwise do not change
      new_val <- seat$value 
    }
  } else { #floor never changes
    new_val <- seat$value
  }
  return(new_val)
}

occupy_seats <- function(dat){
  dat <- parse_input(dat) %>% 
    mutate(rowid = 1:n()) %>% 
    pivot_longer(cols = -rowid) %>% 
    mutate(name = as.numeric(gsub("col", "", name))) %>% 
    rename(colid = name)
  
  pass <- 0
  test <- FALSE
  
  while(any(test == FALSE)){
    print(paste0("pass: ", pass))
    pass_result <- dat %>%
      select(rowid, colid, value) %>% 
      rowwise() %>% 
      mutate(value = seat_mapper(., rowid, colid))
    
    dat <- dat %>% 
      rename(!!paste0("pass", pass) := value) %>% 
      bind_cols(pass_result %>% select(value))
    
    test <- dat[ncol(dat)-1] == dat[ncol(dat)]
    pass <- pass + 1
  }
  
  print(sum(dat[,ncol(dat)] == "#"))
  return(dat)
}

occupy_seats(input_test)

#this works, but it is too slow for the whole input :( 

##make it leaner:

#get the full grid of seats based on the distance we're looking around each seat
get_groups <- function(dat, dist){
  expand.grid(x=1:nrow(dat), y=1:ncol(dat)) %>% 
    rowwise() %>% 
    mutate(rowid = list((x-dist):(x+dist)), 
           colid = list((y-dist):(y+dist))) %>% 
    ungroup() %>% 
    mutate(id = 1:n()) %>% 
    unnest(cols = colid) %>% 
    unnest(cols = rowid) %>% 
    mutate(seat = (colid == y & rowid == x)) 
}


occupy_seats2 <- function(dat, dist){
  #expand so each seat is it's own cell in the table
  dat <- parse_input(dat) 
  
  #change it to long form and remove "floor" values since those never change
  dat2 <- dat %>% 
    mutate(rowid = 1:n()) %>% 
    pivot_longer(cols = -rowid) %>% 
    mutate(name = as.numeric(gsub("col", "", name))) %>% 
    rename(colid = name) %>% 
    filter(value != ".")  
  
  #create the full grid of values with group id and filter to rows/cols in 
  #plane (remove 0s, floor cells, outside grid)
  dat_grid <- get_groups(dat, 1) %>% 
    right_join(dat2, by = c("rowid", "colid")) %>% 
    select(-value)
  
  #join to the full grid to get an id value for each 3x3 area of interest
  dat2 <- dat2 %>% left_join(dat_grid, by = c("rowid", "colid"))
  
  start_vals <- "#"
  passes <- 0
  #testval <- dat2 %>% filter(id == 12) %>% pull(value) %>% unique()
  while(!all(start_vals == dat2$value)){
    passes <- passes + 1
    print(paste0("pass: ", passes))
    start_vals <- dat2$value
    newseats <- dat2 %>% 
      group_by(id) %>% 
      mutate(value = case_when(sum(value == "#") > 4 & value == "#" ~ "L", 
                               !any(value == "#") & value == "L" ~ "#", 
                               TRUE ~ value)) %>% 
      ungroup() %>% 
      filter(seat == TRUE) %>% 
      select(-seat, -id, -x, -y)
    
    dat2 <- newseats %>% 
      full_join(dat_grid, by = c("rowid", "colid")) 
    
    #testval <- c(testval, dat2 %>% filter(id == 12 & seat == TRUE) %>% pull(value) %>% unique())
  }
  
  #result <- c("L", "#", "L", "L", "L", "L", "L")
  
  print(dat2 %>% filter(seat == TRUE & value == "#") %>% nrow())
  return(dat2)
}

out <- occupy_seats2(input_test)
out <- occupy_seats2(input)

####PART 2####
input_test2 <- read.csv("C:/Users/Carlin/Documents/GitHub/adventofcode2020/day11/input_test2.txt", header = FALSE) 
input_test3 <- read.csv("C:/Users/Carlin/Documents/GitHub/adventofcode2020/day11/input_test3.txt", header = FALSE) 
input_test4 <- read.csv("C:/Users/Carlin/Documents/GitHub/adventofcode2020/day11/input_test4.txt", header = FALSE) 

check_subset <- function(dat, row, col){
  if(row %in% c(1:nrow(dat)) & col %in% c(1:ncol(dat))){
    return(as.character(dat[row, col]))
  } else {
    return(as.character("OOB"))
  }
}

go_dir <-  function(dat, x, y, dir, dist){
  switch(dir, 
         "N" = check_subset(dat, x-dist, y), 
         "NE" = check_subset(dat, x-dist, y+dist), 
         "E" = check_subset(dat, x, y+dist), 
         "SE" = check_subset(dat, x+dist, y+dist), 
         "S" = check_subset(dat, x+dist, y), 
         "SW" = check_subset(dat, x+dist, y-dist), 
         "W" = check_subset(dat, x, y-dist), 
         "NW" = check_subset(dat, x-dist, y-dist))
}


seat_in_dir <- function(dat, x, y, dir, max_dist){ 
  seat <- c(NA)
  dist <- 1
  while(dist <= max_dist & !(seat %in% c("L", "#", "OOB"))){
    seat <- go_dir(dat, x, y, dir, dist)
    dist <- dist + 1
  }
  return(seat)
}

dat <- input_test

expanding_seat_test <- function(dat, x, y){
  side_dist <- list("N" = abs(1 - x), 
                    "E" = abs(ncol(dat) - y), 
                    "S" = abs(nrow(dat) - x), 
                    "W" = abs(1 - y)) %>% 
    map_dfr(~ifelse(identical(.x, numeric(0)), 0, .x)) %>% 
    pivot_longer(everything())
  
  max_dist <- max(side_dist$value)
  seatchange <- FALSE
  occ_count <- c()
  dirs <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  for(dir in dirs){
    seat <- seat_in_dir(dat, x, y, dir, max_dist)
    occ_count <- c(occ_count, seat[!is.na(seat)])
    
    if(dat[x,y] == "#" & sum(occ_count == "#") >= 5){
      seatchange <- TRUE
      break
    }
    
    if(dat[x,y] == "L" & sum(occ_count == "#") >= 5){
      seatchange <- FALSE
      break
    }
  }
  
  if(dat[x,y] == "L" & !any(occ_count == "#")){
    seatchange <- TRUE
  }
  return(seatchange)
}

# dat <- input_test2 %>% parse_input(); x <- 5; y <- 4;
# dat <- input_test3 %>% parse_input(); x <- 2; y <- 2;
# dat <-  input_test4 %>% parse_input(); x <- 4; y <- 4;
# expanding_seat_test(dat, x, y)

occupy_seats3 <- function(dat){
  dat <- parse_input(dat) 
  pass <- 1
  changing <- TRUE
  testval <- c()

  while(changing == TRUE){
    print(paste0("pass: ", pass))
    #testval <- c(testval, unlist(c(dat[2,2], use.names = F)))
   
    prev_dat <- dat
    
    dat2 <- dat %>% 
      mutate(rowid = 1:n()) %>% 
      pivot_longer(cols = -rowid) %>% 
      mutate(name = as.numeric(gsub("col", "", name))) %>% 
      rename(colid = name) %>% 
      filter(value != ".")  

    seatchange <- dat2 %>% 
      #filter(rowid == 2, colid == 2) %>% 
      rowwise() %>% 
      mutate(seatchange = expanding_seat_test(dat, rowid, colid),
             new_value = case_when(seatchange == TRUE & value == "L" ~ "#", 
                                   seatchange == TRUE & value == "#" ~ "L",
                                   TRUE ~ value))
    
    for(i in 1:nrow(seatchange)){
      dat[seatchange$rowid[i], seatchange$colid[i]] <- seatchange$new_value[i]
    }
    
    # passdat <- read.csv(paste0("C:/Users/Carlin/Documents/GitHub/adventofcode2020/day11/p", pass, ".txt"), header = FALSE) %>% parse_input()
    # ptest <- all(passdat == dat)
    pass <- pass + 1
    changing <- !all(prev_dat == dat)
  }
  
  print(sum(dat == "#"))
  return(dat)
}

out <- occupy_seats3(input_test)
out <- occupy_seats3(input) #IT'S SO SLOW BUT IT WORKED!!!! and I'm not sure how to make it faster at this point...
#test_true <- c("L", "#", "L", "L", "L", "L", "L") 
