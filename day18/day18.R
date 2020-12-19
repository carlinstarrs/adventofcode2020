# --- Day 18: Operation Order --- 
# As you look out the window and notice a
# heavily-forested continent slowly appear over the horizon, you are interrupted
# by the child sitting next to you. They're curious if you could help them with
# their math homework.
#
# Unfortunately, it seems like this "math" follows different rules than you
# remember.
#
# The homework (your puzzle input) consists of a series of expressions that
# consist of addition (+), multiplication (*), and parentheses ((...)). Just
# like normal math, parentheses indicate that the expression inside must be
# evaluated before it can be used by the surrounding expression. Addition still
# finds the sum of the numbers on both sides of the operator, and multiplication
# still finds the product.
#
# However, the rules of operator precedence have changed. Rather than evaluating
# multiplication before addition, the operators have the same precedence, and
# are evaluated left-to-right regardless of the order in which they appear.
#
# For example, the steps to evaluate the expression 1 + 2 * 3 + 4 * 5 + 6 are as
# follows:
# 
# 1 + 2 * 3 + 4 * 5 + 6
#   3   * 3 + 4 * 5 + 6
#       9   + 4 * 5 + 6
#          13   * 5 + 6
#              65   + 6
#                  71
# Parentheses can override this order; for example, here is what happens if
# parentheses are added to form 1 + (2 * 3) + (4 * (5 + 6)):
# 
# 1 + (2 * 3) + (4 * (5 + 6))
# 1 +    6    + (4 * (5 + 6))
#      7      + (4 * (5 + 6))
#      7      + (4 *   11   )
#      7      +     44
#             51
# Here are a few more examples:
# 
# 2 * 3 + (4 * 5) becomes 26.
# 5 + (8 * 3 + 9 + 3 * 4 * 3) becomes 437.
# 5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4)) becomes 12240.
# ((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2 becomes 13632.
#
# Before you can help with the homework, you need to understand it yourself.
# Evaluate the expression on each line of the homework; what is the sum of the
# resulting values?
library("tidyverse")
library("stringi")
library("rlang")

input <- readLines("C:/Users/Carlin/Documents/GitHub/adventofcode2020/day18/input.txt") 

lr_eval <- function(dat){
  dat <- str_remove_all(dat, "(\\()|(\\))")
  m <- str_split(dat, " ")[[1]]
  i <- 1
  while(length(m) > 1){
    m2 <- eval(parse_expr(paste(m[i:(i+2)], collapse = "")))
    m[i] <- m2
    m <- m[-(i+1):-(1+2)]
  }
  return(m)
}

new_math <- function(dat){
  ps <- str_extract_all(dat, "\\(([^()]|)*\\)") %>% unlist()
  
  while(length(ps) > 0){
    dat <- stri_replace_all_fixed(dat, ps[1], lr_eval(ps[1]))
    ps <- str_extract_all(dat, "\\(([^()]|)*\\)") %>% unlist()
  }
  
  lr_eval(dat)
}

new_math(c("1 + 2 * 3 + 4 * 5 + 6"))
new_math(c("1 + (2 * 3) + (4 * (5 + 6))"))
new_math(c("2 * 3 + (4 * 5)"))
new_math(c("5 + (8 * 3 + 9 + 3 * 4 * 3)"))
new_math(c("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"))
new_math(c("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"))

input %>% 
  map_dbl(~as.numeric(new_math(.x))) %>% 
  sum() %>% 
  sprintf('%.0f', .)


# --- Part Two ---
# You manage to answer the child's questions and they finish part 1 of their
# homework, but get stuck when they reach the next section: advanced math.
# 
# Now, addition and multiplication have different precedence levels, but they're
# not the ones you're familiar with. Instead, addition is evaluated before
# multiplication.
# 
# For example, the steps to evaluate the expression 1 + 2 * 3 + 4 * 5 + 6 are
# now as follows:
# 
# 1 + 2 * 3 + 4 * 5 + 6
#   3   * 3 + 4 * 5 + 6
#   3   *   7   * 5 + 6
#   3   *   7   *  11
#      21       *  11
#          231
# Here are the other examples from above:
# 
# 1 + (2 * 3) + (4 * (5 + 6)) still becomes 51.
# 2 * 3 + (4 * 5) becomes 46.
# 5 + (8 * 3 + 9 + 3 * 4 * 3) becomes 1445.
# 5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4)) becomes 669060.
# ((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2 becomes 23340.
# What do you get if you add up the results of evaluating the homework problems using these new rules?


pm_eval <- function(dat){
  dat <- str_remove_all(dat, "(\\()|(\\))")
  
  ps <- str_extract_all(dat, "\\d{1,} \\+ \\d{1,}") %>% unlist()
  
  while(length(ps) > 0){
    dat <- str_replace(dat, gsub("\\+", "\\\\+", ps[1]), as.character(eval(parse_expr(ps[1]))))
    ps <- str_extract_all(dat, "\\d{1,} \\+ \\d{1,}") %>% unlist()
  }
  
  eval(parse_expr(dat))
}

new_math2 <- function(dat){
  ps <- str_extract_all(dat, "\\(([^()]|)*\\)") %>% unlist()
  
  while(length(ps) > 0){
    dat <- stri_replace_all_fixed(dat, ps[1], pm_eval(ps[1]))
    ps <- str_extract_all(dat, "\\(([^()]|)*\\)") %>% unlist()
  }
  
  pm_eval(dat)
}

new_math2(c("1 + 2 * 3 + 4 * 5 + 6"))
new_math2(c("1 + (2 * 3) + (4 * (5 + 6))"))
new_math2(c("2 * 3 + (4 * 5)"))
new_math2(c("5 + (8 * 3 + 9 + 3 * 4 * 3)"))
new_math2(c("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"))
new_math2(c("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"))

input %>% 
  map_dbl(~as.numeric(new_math2(.x))) %>% 
  sum() %>% 
  sprintf('%.0f', .)