# - Day 5: Binary Boarding --- You board your plane only to discover a new
# problem: you dropped your boarding pass! You aren't sure which seat is yours,
# and all of the flight attendants are busy with the flood of people that
# suddenly made it through passport control.
#
# You write a quick program to use your phone's camera to scan all of the nearby
# boarding passes (your puzzle input); perhaps you can find your seat through
# process of elimination.
#
# Instead of zones or groups, this airline uses binary space partitioning to
# seat people. A seat might be specified like FBFBBFFRLR, where F means "front",
# B means "back", L means "left", and R means "right".
#
# The first 7 characters will either be F or B; these specify exactly one of the
# 128 rows on the plane (numbered 0 through 127). Each letter tells you which
# half of a region the given seat is in. Start with the whole list of rows; the
# first letter indicates whether the seat is in the front (0 through 63) or the
# back (64 through 127). The next letter indicates which half of that region the
# seat is in, and so on until you're left with exactly one row.
#
# For example, consider just the first seven characters of FBFBBFFRLR:
#
# Start by considering the whole range, rows 0 through 127. F means to take the
# lower half, keeping rows 0 through 63. B means to take the upper half, keeping
# rows 32 through 63. F means to take the lower half, keeping rows 32 through
# 47. B means to take the upper half, keeping rows 40 through 47. B keeps rows
# 44 through 47. F keeps rows 44 through 45. The final F keeps the lower of the
# two, row 44. The last three characters will be either L or R; these specify
# exactly one of the 8 columns of seats on the plane (numbered 0 through 7). The
# same process as above proceeds again, this time with only three steps. L means
# to keep the lower half, while R means to keep the upper half.
#
# For example, consider just the last 3 characters of FBFBBFFRLR:
#
# Start by considering the whole range, columns 0 through 7. R means to take the
# upper half, keeping columns 4 through 7. L means to take the lower half,
# keeping columns 4 through 5. The final R keeps the upper of the two, column 5.
# So, decoding FBFBBFFRLR reveals that it is the seat at row 44, column 5.
#
# Every seat also has a unique seat ID: multiply the row by 8, then add the
# column. In this example, the seat has ID 44 * 8 + 5 = 357.
#
# Here are some other boarding passes:
#
# BFFFBBFRRR: row 70, column 7, seat ID 567. FFFBBBFRRR: row 14, column 7, seat
# ID 119. BBFFBBFRLL: row 102, column 4, seat ID 820. As a sanity check, look
# through your list of boarding passes. What is the highest seat ID on a
# boarding pass?


####PART 1####
input <- c(unlist(read.csv("C:/Users/Carlin/Documents/GitHub/adventofcode2020/day05/input.txt", header = FALSE)), use.names = F)

###TEST 1.1###
library("tidyverse")

test_input <- c("FBFBBFFRLR", "BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL")
seat_rows <- c(0,127)
seat_cols <- c(0,7)

find_seats <- function(seat_array, seat_rows, seat_cols){
  get_seat <- function(bin, x, y){  
    seat_min <- switch(bin, 
                       "upper" = x, 
                       "lower" = (y-x)/2 + x) %>% ceiling()
    
    seat_max <- switch(bin, 
                       "upper" = (y-x)/2 + x, 
                       "lower" = y) %>% floor()
    
    return(c(seat_min, seat_max))
  }
  
  final_row <- c()
  final_col <- c()
  
  for(i in 1:nchar(seat_array)){
    bin <- str_sub(seat_array, i, i) 
    
    if(i == 1 | i == 8){
      if(bin %in% c("F", "B")){
        seats <- seat_rows
      } else {
        seats <- seat_cols
      }
    }
    
    if(bin %in% c("F", "L")){
      bin <- "upper"
    } else {
      bin <- "lower"
    }
    
    x <- seats[1]
    y <- seats[2]
    
    seats <- get_seat(bin, x, y)
    
    if(i == 7){
      final_row <- unique(seats)
    } else if (i == 10){
      final_col <- unique(seats)
    }
    
    #print(paste(i, seats[1], seats[2]))
  }
  
  return(data.frame("SEAT_ARRAY" = seat_array,
                    "FINAL_ROW" = final_row, 
                    "FINAL_COL" = final_col, 
                    "SEAT_ID" = final_row * 8 + final_col))
}


out <- map_dfr(test_input, ~find_seats(.x, seat_rows, seat_cols))
out <-  map_dfr(input, ~find_seats(.x, seat_rows, seat_cols))
max(out$SEAT_ID)

####PART 2#### Ding! The "fasten seat belt" signs have turned on. Time to find
#your seat.
#
#It's a completely full flight, so your seat should be the only missing boarding
#pass in your list. However, there's a catch: some of the seats at the very
#front and back of the plane don't exist on this aircraft, so they'll be missing
#from your list as well.
#
#Your seat wasn't at the very front or back, though; the seats with IDs +1 and
#-1 from yours will be in your list.

# What is the ID of your seat?
all_ids <- data.frame("FINAL_COL" = rep(0:127, times = length(0:7))) %>% 
  arrange(FINAL_COL) %>% 
   mutate("FINAL_ROW" = rep(0:7, length.out = nrow(.)), 
          "SEAT_ID" = FINAL_COL * 8 + FINAL_ROW) %>% 
  select(SEAT_ID)

out2 <- out %>% 
  full_join(all_ids) %>% 
  arrange(SEAT_ID) %>% 
  mutate(ABOVE = is.na(lead(SEAT_ARRAY, 1)), 
         BELOW = is.na(lag(SEAT_ARRAY, 1))) %>%
  filter(is.na(SEAT_ARRAY) & ABOVE == FALSE & BELOW == FALSE)

