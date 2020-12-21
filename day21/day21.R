# --- Day 21: Allergen Assessment --- 
# You reach the train's last stop and the
# closest you can get to your vacation island without getting wet. There aren't
# even any boats here, but nothing can stop you now: you build a raft. You just
# need a few days' worth of food for your journey.
#
# You don't speak the local language, so you can't read any ingredients lists.
# However, sometimes, allergens are listed in a language you do understand. You
# should be able to use this information to determine which ingredient contains
# which allergen and work out which foods are safe to take with you on your
# trip.
#
# You start by compiling a list of foods (your puzzle input), one food per line.
# Each line includes that food's ingredients list followed by some or all of the
# allergens the food contains.
# 
# Each allergen is found in exactly one ingredient. Each ingredient contains
# zero or one allergen. Allergens aren't always marked; when they're listed (as
# in (contains nuts, shellfish) after an ingredients list), the ingredient that
# contains each listed allergen will be somewhere in the corresponding
# ingredients list. However, even if an allergen isn't listed, the ingredient
# that contains that allergen could still be present: maybe they forgot to label
# it, or maybe it was labeled in a language you don't know.
#
# For example, consider the following list of foods:
#   
# mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
# trh fvjkl sbzzf mxmxvkd (contains dairy)
# sqjhc fvjkl (contains soy)
# sqjhc mxmxvkd sbzzf (contains fish)
# 
# The first food in the list has four ingredients (written in a language you
# don't understand): mxmxvkd, kfcds, sqjhc, and nhms. While the food might
# contain other allergens, a few allergens the food definitely contains are
# listed afterward: dairy and fish.
#
# The first step is to determine which ingredients can't possibly contain any of
# the allergens in any food in your list. In the above example, none of the
# ingredients kfcds, nhms, sbzzf, or trh can contain an allergen. Counting the
# number of times any of these ingredients appear in any ingredients list
# produces 5: they all appear once each except sbzzf, which appears twice.
#
# Determine which ingredients cannot possibly contain any of the allergens in
# your list. How many times do any of those ingredients appear?
library("tidyverse")

input_test <- c("mxmxvkd kfcds sqjhc nhms (contains dairy, fish)",
                "trh fvjkl sbzzf mxmxvkd (contains dairy)",
                "sqjhc fvjkl (contains soy)",
                "sqjhc mxmxvkd sbzzf (contains fish)")

input <- readLines("C:/Users/Carlin/Documents/GitHub/adventofcode2020/day21/input.txt")

parse_input <- function(dat){
  dat %>% 
    data.frame() %>% 
    rename("ingredients" = 1) %>%
    mutate(food_id = 1:n(),
           allergens = str_extract(ingredients, "\\(([^)]+)\\)"), 
           ingredients = trimws(str_remove(ingredients, "\\(([^)]+)\\)")), 
           allergens = gsub("\\(|\\)|contains| ", "", allergens)) %>%
    separate_rows(allergens, sep = ",") %>% 
    separate_rows(ingredients, sep = " ") %>% 
    # group_by(food_id) %>% 
    # mutate(i_per_f = n_distinct(ingredients), 
    #        a_per_f =  n_distinct(allergens)) %>% 
    # ungroup() %>% 
    # group_by(ingredients) %>% 
    # mutate(f_per_i = n_distinct(food_id), 
    #        a_per_i = n_distinct(allergens)) %>% 
    # ungroup() %>% 
    # group_by(allergens) %>% 
    # mutate(f_per_a = n_distinct(food_id), 
    #        i_per_a = n_distinct(ingredients)) %>% 
    # ungroup() %>% 
    filter(!ingredients == "")
  
}

get_potentials <- function(dat){
  allergens <- dat %>% pull(allergens) %>% unique()
  map_dfr(allergens, function(a){
    pots <- data.frame()
    ingredients <- dat %>% filter(allergens == a) %>% pull(ingredients) %>% unique()
    foods <- dat %>% filter(allergens == a) %>% pull(food_id) %>% unique() %>% length()
    
    pot <- map(ingredients, function(ingredient){
      test <- dat %>% 
        filter(allergens == a) %>%
        group_by(food_id) %>% 
        summarise(TEST = any(ingredients == ingredient), .groups = "drop") %>% 
        filter(sum(TEST) == foods)
      
      if(nrow(test) > 0){
        return(ingredient)
      } else {
        return(NULL)
      }
    }) %>% 
      compact() %>% 
      unlist()
    
    pots <- bind_rows(pots, data.frame("pot" = pot, "allergen" = rep(a, length(pot))))
  })
}

#dat <- parse_input(input_test) 
dat <- parse_input(input)

dat2 <- dat
ss <- data.frame()
while(nrow(ss) < length(unique(dat$allergens))){
  potentials <- get_potentials(dat2)
  singles <- potentials %>% group_by(allergen) %>% mutate(n = n_distinct(pot)) %>% filter(n == 1)
  ss <- bind_rows(ss, singles)
  dat2 <- dat2 %>% 
    filter(!allergens %in% ss$allergen) %>% 
    filter(!ingredients %in% ss$pot)
}

#Part 1
dat %>% 
  filter(!ingredients %in% ss$pot) %>% 
  group_by(ingredients) %>%
  summarise(n = n_distinct(food_id), .groups = "drop") %>% 
  pull(n) %>% 
  sum()

#Part 2
ss %>% 
  arrange(allergen) %>% 
  summarise(m = paste(pot, collapse = ",")) %>% 
  print()
