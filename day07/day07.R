# --- Day 7: Handy Haversacks --- 
# You land at the regional airport in time for
# your next flight. In fact, it looks like you'll even have time to grab some
# food: all flights are currently delayed due to issues in luggage processing.
#
# Due to recent aviation regulations, many rules (your puzzle input) are being
# enforced about bags and their contents; bags must be color-coded and must
# contain specific quantities of other color-coded bags. Apparently, nobody
# responsible for these regulations considered how long they would take to
# enforce!
#
# For example, consider the following rules:
#
# light red bags contain 1 bright white bag, 2 muted yellow bags. 
# dark orange bags contain 3 bright white bags, 4 muted yellow bags. 
# bright white bags contain 1 shiny gold bag. 
# muted yellow bags contain 2 shiny gold bags, 9 faded blue bags. 
# shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags. 
# dark olive bags contain 3 faded blue bags, 4 dotted black bags. 
# vibrant plum bags contain 5 faded blue bags, 6 dotted black bags. 
# faded blue bags contain no other bags. 
# dotted black bags contain no other bags. 

# These rules specify the required contents for 9 bag types. In this example, 
# every faded blue bag is empty, every vibrant plum bag contains 11 bags 
# (5 faded blue and 6 dotted black), and so on.
#
# You have a shiny gold bag. If you wanted to carry it in at least one other
# bag, how many different bag colors would be valid for the outermost bag? (In
# other words: how many colors can, eventually, contain at least one shiny gold
# bag?)
#
# In the above rules, the following options would be available to you:
#
# A bright white bag, which can hold your shiny gold bag directly. 
# A muted yellow bag, which can hold your shiny gold bag directly, plus some other bags.
# A dark orange bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag. 
# A light red bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny
# gold bag. So, in this example, the number of bag colors that can eventually
# contain at least one shiny gold bag is 4.
#
# How many bag colors can eventually contain at least one shiny gold bag? (The
# list of rules is quite long; make sure you get all of it.)

library("tidyverse")
####PART 1####
input_test <-  readLines("C:/Users/Carlin/Documents/GitHub/adventofcode2020/day07/input_test.txt")
input <-  readLines("C:/Users/Carlin/Documents/GitHub/adventofcode2020/day07/input.txt")

filter_bag <- function(dat, bag_color){
  dat[unlist(map(dat, ~str_detect(.x, bag_color)))] %>% map_chr(~word(.x, 1, 2))
}


get_all_bags <- function(dat, start_bag){
  all_bags <- c()
  new_bags <- start_bag

  while(length(new_bags > 0)){
    valid_bags <- unlist(map(new_bags, ~filter_bag(dat, .x))) %>% unique()
    new_bags <- valid_bags[!valid_bags %in% c(start_bag, all_bags)]
    all_bags <- c(all_bags, valid_bags)
  }
  
  return(length(unique(all_bags[all_bags != start_bag])))
}

get_all_bags(input_test, "shiny gold")
get_all_bags(input, "shiny gold")


# -- Part Two --- 
# It's getting pretty expensive to fly these days - not because
# of ticket prices, but because of the ridiculous number of bags you need to
# buy!
#
# Consider again your shiny gold bag and the rules from the above example:
#
# faded blue bags contain 0 other bags. 
# dotted black bags contain 0 other bags.
# vibrant plum bags contain 11 other bags: 5 faded blue bags and 6 dotted black bags. 
# dark olive bags contain 7 other bags: 3 faded blue bags and 4 dotted black bags.
#
# So, a single shiny gold bag must contain 1 dark olive bag (and the
# 7 bags within it) plus 2 vibrant plum bags (and the 11 bags within each of
# those): 1 + 1*7 + 2 + 2*11 = 32 bags!
#
# Of course, the actual rules have a small chance of going several levels deeper
# than this example; be sure to count all of the bags, even if the nesting
# becomes topologically impractical!
#
# Here's another example:
# 
# shiny gold bags contain 2 dark red bags.
# dark red bags contain 2 dark orange bags.
# dark orange bags contain 2 dark yellow bags.
# dark yellow bags contain 2 dark green bags.
# dark green bags contain 2 dark blue bags.
# dark blue bags contain 2 dark violet bags.
# dark violet bags contain no other bags.
# In this example, a single shiny gold bag must contain 126 other bags.

# How many individual bags are required inside your single shiny gold bag?

get_bag_nums <- function(dat){
  data.frame("X1" = dat) %>% 
    mutate(id = 1:n(), 
           bag_color = word(X1, 1, 2)) %>% 
    separate_rows(X1, sep = " bags contain ") %>% 
    separate_rows(X1, sep = ", ") %>% 
    mutate(X1 = trimws(gsub("bag(|s)|\\.", "", X1)), 
           num_bags = str_extract(X1, "^\\d{1,}"), 
           X1 = trimws(str_remove(X1, "^\\d{1,}")), 
           num_bags = as.numeric(num_bags)) %>% 
    filter(!is.na(num_bags)) 
}

get_bag_count <- function(dat, start_bag){
  bag_db <- get_bag_nums(dat)
  
  all_bags <- c()
  new_bags <- start_bag
  bag_counter <- c()
  while(!is.null(new_bags)){
    bags_in_bag <- map_dfr(new_bags, ~filter(bag_db, bag_color == .x))
    bag_counter <- sum(bag_counter, bags_in_bag$num_bags)
    new_bags <- pmap(bags_in_bag, function(...){
      bags <- data.frame(...)
      rep(bags$X1, as.numeric(bags$num_bags))
    }) %>% unlist()
  }
  
  return(bag_counter)
}

get_bag_count(input_test, "shiny gold")
get_bag_count(input, "shiny gold")
