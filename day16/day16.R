# --- Day 16: Ticket Translation ---
# As you're walking to yet another connecting flight, you realize that one of
# the legs of your re-routed trip coming up is on a high-speed train. However,
# the train ticket you were given is in a language you don't understand. You
# should probably figure out what it says before you get to the train station
# after the next flight.
# 
# Unfortunately, you can't actually read the words on the ticket. You can,
# however, read the numbers, and so you figure out the fields these tickets must
# have and the valid ranges for values in those fields.
# 
# You collect the rules for ticket fields, the numbers on your ticket, and the
# numbers on other nearby tickets for the same train service (via the airport
# security cameras) together into a single document you can reference (your
# puzzle input).
# 
# The rules for ticket fields specify a list of fields that exist somewhere on
# the ticket and the valid ranges of values for each field. For example, a rule
# like class: 1-3 or 5-7 means that one of the fields in every ticket is named
# class and can be any value in the ranges 1-3 or 5-7 (inclusive, such that 3
# and 5 are both valid in this field, but 4 is not).
# 
# Each ticket is represented by a single line of comma-separated values. The
# values are the numbers on the ticket in the order they appear; every ticket
# has the same format. For example, consider this ticket:
# 
# .--------------------------------------------------------.
# | ????: 101    ?????: 102   ??????????: 103     ???: 104 |
# |                                                        |
# | ??: 301  ??: 302             ???????: 303      ??????? |
# | ??: 401  ??: 402           ???? ????: 403    ????????? |
# '--------------------------------------------------------'
#
# Here, ? represents text in a language you don't understand. This ticket might
# be represented as 101,102,103,104,301,302,303,401,402,403; of course, the
# actual train tickets you're looking at are much more complicated. In any case,
# you've extracted just the numbers in such a way that the first number is
# always the same specific field, the second number is always a different
# specific field, and so on - you just don't know what each position actually
# means!
#
# Start by determining which tickets are completely invalid; these are tickets
# that contain values which aren't valid for any field. Ignore your ticket for
# now.
#
# For example, suppose you have the following notes:
# class: 1-3 or 5-7
# row: 6-11 or 33-44
# seat: 13-40 or 45-50
# 
# your ticket:
# 7,1,14
# 
# nearby tickets:
# 7,3,47
# 40,4,50
# 55,2,20
# 38,6,12
# 
# It doesn't matter which position corresponds to which field; you can identify
# invalid nearby tickets by considering only whether tickets contain values that
# are not valid for any field. In this example, the values on the first nearby
# ticket are all valid for at least one field. This is not true of the other
# three nearby tickets: the values 4, 55, and 12 are are not valid for any
# field. Adding together all of the invalid values produces your ticket scanning
# error rate: 4 + 55 + 12 = 71.
#
# Consider the validity of the nearby tickets you scanned. What is your ticket
# scanning error rate?

input_test <-  readLines("C:/Users/Carlin/Documents/GitHub/adventofcode2020/day16/input_test.txt")
input_test2 <-  readLines("C:/Users/Carlin/Documents/GitHub/adventofcode2020/day16/input_test2.txt")
input <- readLines("C:/Users/Carlin/Documents/GitHub/adventofcode2020/day16/input.txt")

dat <- input_test

parse_input <- function(dat){
  my_ticket <- as.numeric(str_split(dat[which(grepl("your ticket:", dat)) + 1], ",")[[1]])
  
  nearby_tickets <- map(str_split(dat[(which(grepl("nearby tickets:", dat)) + 1):length(dat)], ","), as.numeric)
  
  fields <- dat[1:(which(grepl("your ticket:", dat)) - 2)] %>% 
    tibble("field" = .) %>% 
    separate(field, sep = ": ", into = c("field", "value")) %>% 
    separate(value, sep = " or ", into = c("r1", "r2")) %>% 
    separate(r2, sep = "-", into = c("r3", "r4")) %>% 
    separate(r1, sep = "-", into = c("r1", "r2")) %>% 
    rowwise() %>% 
    mutate(values = list(c(r1:r2, r3:r4))) %>% 
    select(field, values)
  
  return(list("my_ticket" = my_ticket, 
              "nearby_tickets" = nearby_tickets, 
              "fields" = fields))
}


error_rate <- function(nearby_tickets, fields){
  map(nearby_tickets, ~.x[!.x %in% unlist(fields$values)]) %>% 
    compact() %>% 
    unlist() %>% 
    sum(.)
}

dat <- parse_input(input_test)
dat <- parse_input(input)
error_rate(dat$nearby_tickets, dat$fields)

# --- Part Two ---
# Now that you've identified which tickets contain invalid values, discard those
# tickets entirely. Use the remaining valid tickets to determine which field is
# which.
# 
# Using the valid ranges for each field, determine what order the fields appear
# on the tickets. The order is consistent between all tickets: if seat is the
# third field, it is the third field on every ticket, including your ticket.
# 
# For example, suppose you have the following notes:
# 
# class: 0-1 or 4-19
# row: 0-5 or 8-19
# seat: 0-13 or 16-19
# 
# your ticket:
# 11,12,13
# 
# nearby tickets:
# 3,9,18
# 15,1,5
# 5,14,9
# 
# Based on the nearby tickets in the above example, the first position must be
# row, the second position must be class, and the third position must be seat;
# you can conclude that in your ticket, class is 12, row is 11, and seat is 13.
# 
# Once you work out which field is which, look for the six fields on your ticket
# that start with the word departure. What do you get if you multiply those six
# values together?

validate <- function(nearby_tickets, fields){
  nearby_tickets[which(map(nearby_tickets, ~all(.x %in% unlist(fields$values))) == TRUE)]
}


field_mapper <- function(dat){
  dat <- parse_input(dat)
  
  fields <- dat$fields %>% unnest(values)
  
  valid_tickets <- validate(dat$nearby_tickets, dat$fields)
  
  nb <- do.call(rbind.data.frame, valid_tickets) 
  names(nb) <- paste0("f", 1:ncol(nb))
  
  field_count <- nb %>%
    mutate(ticket = 1:n()) %>% 
    pivot_longer(cols = contains("f"), 
                 names_to = "unk_field", 
                 values_to = "values") %>%
    left_join(fields, by = "values") %>% 
    group_by(unk_field, field) %>%
    tally() %>% 
    filter(n == max(n)) %>% 
    select(-n) %>% 
    group_by(unk_field) %>% 
    mutate(n = length(unique(field))) %>% 
    ungroup()
  
  singles <- field_count %>% filter(n == 1)
  
  while(nrow(singles) < nrow(dat$fields)){
    field_count <- field_count %>% 
      filter(!field %in% singles$field) %>% 
      group_by(unk_field) %>% 
      mutate(n = length(unique(field))) %>% 
      ungroup()
    
    singles <- singles %>% bind_rows(field_count %>% filter(n == 1))
  }
  
  return(singles %>% mutate(field_id = as.numeric(gsub("f", "", unk_field))) %>% arrange(field_id))
}

dat <- input_test2

dat <- input
fm <- field_mapper(dat)
out <- data.frame("ticket" = parse_input(dat)$my_ticket, 
                  "field" = fm$field)

out %>% 
  filter(grepl("departure", field)) %>% 
  pull(ticket) %>% 
  prod() %>% 
  sprintf('%.0f', .)
