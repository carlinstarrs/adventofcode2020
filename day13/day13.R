# --- Day 13: Shuttle Search --- 
# Your ferry can make it safely to a nearby port,
# but it won't get much further. When you call to book another ship, you
# discover that no ships embark from that port to your vacation island. You'll
# need to get from the port to the nearest airport.
#
# Fortunately, a shuttle bus service is available to bring you from the sea port
# to the airport! Each bus has an ID number that also indicates how often the
# bus leaves for the airport.
#
# Bus schedules are defined based on a timestamp that measures the number of
# minutes since some fixed reference point in the past. At timestamp 0, every
# bus simultaneously departed from the sea port. After that, each bus travels to
# the airport, then various other locations, and finally returns to the sea port
# to repeat its journey forever.
#
# The time this loop takes a particular bus is also its ID number: the bus with
# ID 5 departs from the sea port at timestamps 0, 5, 10, 15, and so on. The bus
# with ID 11 departs at 0, 11, 22, 33, and so on. If you are there when the bus
# departs, you can ride that bus to the airport!
#
# Your notes (your puzzle input) consist of two lines. The first line is your
# estimate of the earliest timestamp you could depart on a bus. The second line
# lists the bus IDs that are in service according to the shuttle company;
# entries that show x must be out of service, so you decide to ignore them.
#
# To save time once you arrive, your goal is to figure out the earliest bus you
# can take to the airport. (There will be exactly one such bus.)
#
# For example, suppose you have the following notes:
#   
# 939
# 7,13,x,x,59,x,31,19
# 
# Here, the earliest timestamp you could depart is 939, and the bus IDs in
# service are 7, 13, 59, 31, and 19. Near timestamp 939, these bus IDs depart at
# the times marked D:
#   
# time   bus 7   bus 13  bus 59  bus 31  bus 19
# 929      .       .       .       .       .
# 930      .       .       .       D       .
# 931      D       .       .       .       D
# 932      .       .       .       .       .
# 933      .       .       .       .       .
# 934      .       .       .       .       .
# 935      .       .       .       .       .
# 936      .       D       .       .       .
# 937      .       .       .       .       .
# 938      D       .       .       .       .
# 939      .       .       .       .       .
# 940      .       .       .       .       .
# 941      .       .       .       .       .
# 942      .       .       .       .       .
# 943      .       .       .       .       .
# 944      .       .       D       .       .
# 945      D       .       .       .       .
# 946      .       .       .       .       .
# 947      .       .       .       .       .
# 948      .       .       .       .       .
# 949      .       D       .       .       .
# 
# The earliest bus you could take is bus ID 59. It doesn't depart until
# timestamp 944, so you would need to wait 944 - 939 = 5 minutes before it
# departs. Multiplying the bus ID by the number of minutes you'd need to wait
# gives 295.
#
# What is the ID of the earliest bus you can take to the airport multiplied by
# the number of minutes you'll need to wait for that bus?


library("tidyverse")

input_test <- c("939", "7,13,x,x,59,x,31,19")
input <-  readLines("C:/Users/Carlin/Documents/GitHub/adventofcode2020/day13/input.txt")

get_closest_next_bus <- function(dat){
  mytime <- as.numeric(dat[1])
  bus_ids <- as.numeric(str_split(dat[2], ",")[[1]][str_split(dat[2], ",")[[1]] != "x"])
  bus_times <- map_dbl(bus_ids, ~ceiling(mytime/.x)*.x-mytime)
  bus_ids[which(bus_times == min(bus_times))] * min(bus_times)
}

get_closest_next_bus(input_test)
get_closest_next_bus(input)

# --- Part Two ---
# The shuttle company is running a contest: one gold coin for anyone that can
# find the earliest timestamp such that the first bus ID departs at that time
# and each subsequent listed bus ID departs at that subsequent minute. (The
# first line in your input is no longer relevant.)
# 
# For example, suppose you have the same list of bus IDs as above:
#   
# 7,13,x,x,59,x,31,19
# An x in the schedule means there are no constraints on what bus IDs must depart at that time.
# 
# This means you are looking for the earliest timestamp (called t) such that:
#   
# Bus ID 7 departs at timestamp t.
# Bus ID 13 departs one minute after timestamp t.
# There are no requirements or restrictions on departures at two or three minutes after timestamp t.
# Bus ID 59 departs four minutes after timestamp t.
# There are no requirements or restrictions on departures at five minutes after timestamp t.
# Bus ID 31 departs six minutes after timestamp t.
# Bus ID 19 departs seven minutes after timestamp t.
#
# The only bus departures that matter are the listed bus IDs at their specific
# offsets from t. Those bus IDs can depart at other times, and other bus IDs can
# depart at those times. For example, in the list above, because bus ID 19 must
# depart seven minutes after the timestamp at which bus ID 7 departs, bus ID 7
# will always also be departing with bus ID 19 at seven minutes after timestamp
# t.
# 
# In this example, the earliest timestamp at which this occurs is 1068781:

input_test2 <- c("", "17,x,13,19")
input_test3 <- c("", "67,7,59,61")
input_test4 <- c("", "67,x,7,59,61")
input_test5 <- c("", "67,7,x,59,61")
input_test6 <- c("", "1789,37,47,1889")

find_bus <- function(dat){
  bus_ids <- as.numeric(str_split(dat[2], ",")[[1]])
  start_bus <- bus_ids[1]
  bus_ids <- bus_ids[-1]
  t_range <- 0:start_bus
  loop <- TRUE
  while(loop == TRUE){
    test <- map_dbl(bus_ids[!is.na(bus_ids)], ~t_range[which(bus_ids == .x) + 1] %% .x)
    if((length(test[!is.na(test)]) == length(test))& all(test[!is.na(test)] == 0)){
      out <- t_range[1]
      loop <- FALSE
    } else {
      loop <- TRUE
    }
    t_range <- t_range + start_bus
  }
  return(out)
}

find_bus(input_test)
find_bus(input_test2)
find_bus(input_test3)
find_bus(input_test4)
find_bus(input_test5)
find_bus(input_test6)

# The tests all pass, but it's too slow for the input..again...
# Turns out this is best solved using the Chinese remainder theorem
# https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Search_by_sieving
# https://www.reddit.com/r/adventofcode/comments/kc4njx/2020_day_13_solutions/gfoerps/
dat <- input
id <- as.numeric(str_split(dat[2], ",")[[1]])
shift <- 1:length(id)

shift <- shift[!is.na(id)] - 1
id <- id[!is.na(id)]

t <- 0
inc <- 1

for(i in 1:length(id)){
  while((t + shift[i])%%id[i] != 0){ #test if remainder of t+shift / bus_id = 0 (I had this above, but the trick was the incrementation)
    t <- t + inc #if not, add an amount 
  }
  inc <- inc * id[i] #This is the CRT part -- you can jump the next increment by multiplying it by the previous id 
}

sprintf('%.0f', t)
