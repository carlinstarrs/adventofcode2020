# --- Day 20: Jurassic Jigsaw --- 
# The high-speed train leaves the forest and
# quickly carries you south. You can even see a desert in the distance! Since
# you have some spare time, you might as well see if there was anything
# interesting in the image the Mythical Information Bureau satellite captured.
#
# After decoding the satellite messages, you discover that the data actually
# contains many small images created by the satellite's camera array. The camera
# array consists of many cameras; rather than produce a single square image,
# they produce many smaller square image tiles that need to be reassembled back
# into a single image.
#
# Each camera in the camera array returns a single monochrome image tile with a
# random unique ID number. The tiles (your puzzle input) arrived in a random
# order.
#
# Worse yet, the camera array appears to be malfunctioning: each image tile has
# been rotated and flipped to a random orientation. Your first task is to
# reassemble the original image by orienting the tiles so they fit together.
#
# To show how the tiles should be reassembled, each tile's image data includes a
# border that should line up exactly with its adjacent tiles. All tiles have
# this border, and the border lines up exactly when the tiles are both oriented
# correctly. Tiles at the edge of the image also have this border, but the
# outermost edges won't line up with any other tiles.
#
# For example, suppose you have the following nine tiles:
# 
# Tile 2311:
# ..##.#..#.
# ##..#.....
# #...##..#.
# ####.#...#
# ##.##.###.
# ##...#.###
# .#.#.#..##
# ..#....#..
# ###...#.#.
# ..###..###
# 
# Tile 1951:
# #.##...##.
# #.####...#
# .....#..##
# #...######
# .##.#....#
# .###.#####
# ###.##.##.
# .###....#.
# ..#.#..#.#
# #...##.#..
# 
# Tile 1171:
# ####...##.
# #..##.#..#
# ##.#..#.#.
# .###.####.
# ..###.####
# .##....##.
# .#...####.
# #.##.####.
# ####..#...
# .....##...
# 
# Tile 1427:
# ###.##.#..
# .#..#.##..
# .#.##.#..#
# #.#.#.##.#
# ....#...##
# ...##..##.
# ...#.#####
# .#.####.#.
# ..#..###.#
# ..##.#..#.
# 
# Tile 1489:
# ##.#.#....
# ..##...#..
# .##..##...
# ..#...#...
# #####...#.
# #..#.#.#.#
# ...#.#.#..
# ##.#...##.
# ..##.##.##
# ###.##.#..
# 
# Tile 2473:
# #....####.
# #..#.##...
# #.##..#...
# ######.#.#
# .#...#.#.#
# .#########
# .###.#..#.
# ########.#
# ##...##.#.
# ..###.#.#.
# 
# Tile 2971:
# ..#.#....#
# #...###...
# #.#.###...
# ##.##..#..
# .#####..##
# .#..####.#
# #..#.#..#.
# ..####.###
# ..#.#.###.
# ...#.#.#.#
# 
# Tile 2729:
# ...#.#.#.#
# ####.#....
# ..#.#.....
# ....#..#.#
# .##..##.#.
# .#.####...
# ####.#.#..
# ##.####...
# ##..#.##..
# #.##...##.
# 
# Tile 3079:
# #.#.#####.
# .#..######
# ..#.......
# ######....
# ####.#..#.
# .#...#.##.
# #.#####.##
# ..#.###...
# ..#.......
# ..#.###...

# By rotating, flipping, and rearranging them, you can find a square arrangement
# that causes all adjacent borders to line up:
# 
# #...##.#.. ..###..### #.#.#####.
# ..#.#..#.# ###...#.#. .#..######
# .###....#. ..#....#.. ..#.......
# ###.##.##. .#.#.#..## ######....
# .###.##### ##...#.### ####.#..#.
# .##.#....# ##.##.###. .#...#.##.
# #...###### ####.#...# #.#####.##
# .....#..## #...##..#. ..#.###...
# #.####...# ##..#..... ..#.......
# #.##...##. ..##.#..#. ..#.###...
# 
# #.##...##. ..##.#..#. ..#.###...
# ##..#.##.. ..#..###.# ##.##....#
# ##.####... .#.####.#. ..#.###..#
# ####.#.#.. ...#.##### ###.#..###
# .#.####... ...##..##. .######.##
# .##..##.#. ....#...## #.#.#.#...
# ....#..#.# #.#.#.##.# #.###.###.
# ..#.#..... .#.##.#..# #.###.##..
# ####.#.... .#..#.##.. .######...
# ...#.#.#.# ###.##.#.. .##...####
# 
# ...#.#.#.# ###.##.#.. .##...####
# ..#.#.###. ..##.##.## #..#.##..#
# ..####.### ##.#...##. .#.#..#.##
# #..#.#..#. ...#.#.#.. .####.###.
# .#..####.# #..#.#.#.# ####.###..
# .#####..## #####...#. .##....##.
# ##.##..#.. ..#...#... .####...#.
# #.#.###... .##..##... .####.##.#
# #...###... ..##...#.. ...#..####
# ..#.#....# ##.#.#.... ...##.....
# For reference, the IDs of the above tiles are:
# 
# 1951    2311    3079
# 2729    1427    2473
# 2971    1489    1171
#
# To check that you've assembled the image correctly, multiply the IDs of the
# four corner tiles together. If you do this with the assembled tiles from the
# example above, you get 1951 * 3079 * 2971 * 1171 = 20899048083289.
#
# Assemble the tiles into an image. What do you get if you multiply together the
# IDs of the four corner tiles?
# 


library("tidyverse")
library("Thermimage")

input_test <- readLines("C:/Users/Carlin/Documents/GitHub/adventofcode2020/day20/input_test.txt")
input <- readLines("C:/Users/Carlin/Documents/GitHub/adventofcode2020/day20/input.txt")

testdat <- data.frame("V1" = input_test) %>% 
  mutate(tile = str_extract(V1, "\\b\\d{1,}\\b")) %>% 
  fill(tile, .direction = "down") %>% 
  filter(!grepl("Tile", V1)) %>% 
  filter(V1 != "")

dat2 <- testdat %>% 
  separate(V1, sep = "", into = paste0("col", 1:(nchar(.[2,1]) +  1))) %>% 
  select(-col1) 

rotate_tiles <- list("r0" = function(m) m,
                     "r90" = function(m) rotate90.matrix(m), 
                     "r180" = function(m) rotate180.matrix(m), 
                     "r270" = function(m) rotate270.matrix(m))

flip_tiles <- list("flip" = function(m) flip.matrix(m), 
                   "mirror" = function(m) mirror.matrix(m))

get_all_tiles <- function(tile){
  tile <- as.matrix(tile, nrow = nrow(tile), ncol = ncol(tile)) #to matrix
  tile[2:(nrow(tile)-1),2:(ncol(tile)-1)] <- NA #remove center
  rotated <- map(rotate_tiles, ~exec(.x, m = tile))
  flipped <- map(flip_tiles, ~exec(.x, m = tile))
  rotated_flipped <- map(rotated[c("r90", "r270")], ~exec(flip_tiles[[1]], m = .x))
  names(rotated_flipped) <- paste0(names(rotated_flipped), "flip")
  
  allt <- c(rotated, flipped, rotated_flipped)
  return(allt)
}


all <- dat2 %>% 
  split(.$tile) %>% 
  map(function(tile){get_all_tiles(tile %>% select(-tile))})

tile_palooza <- data.frame()
for(i in 1:length(all)){
  atile <- all[[i]]
  btile <- all[(1:length(all))[-i]]
  out <- map(atile, function(a) {
    map(btile, function(flipflops) {
      map(flipflops, function(b){
        #get which a borders match which b borders 
        #1951 -> 2729
        #expect 1951 flip bottom = 2729 flip top
        mm <- NULL
        if(all(a[1,] == b[nrow(b),])) mm <- "top"
        if(all(a[nrow(a),] == b[1,])) mm <- "bottom"
        if(all(a[,1] == b[,ncol(b)])) mm <- "right"
        if(all(a[,ncol(a)] == b[,1])) mm <- "left"
        return(list(mm))
      }) %>% compact()
    }) %>% compact() 
  })
  
  names(out) <- paste0(names(all)[i], "_", names(out))
  
  a <- unlist(out) 
  a <- tibble("name" = names(a), 
              "border" = c(a, use.names = FALSE)) %>% 
    separate(name, sep = "\\_", into = c("atile", "name")) %>% 
    separate(name, sep = "\\.", into = c("atdir", "btile", "btdir"))
  tile_palooza = bind_rows(tile_palooza, a)
}

m <- tile_palooza %>% 
  mutate(atdir = str_remove(atdir, "^\\d{1,}_")) %>% 
  group_by(atile) %>% 
  mutate(matches = n_distinct(btile)) %>% 
  group_by(atile, atdir) %>% 
  mutate(allsides = n_distinct(border)) %>% 
  ungroup()

corners <- unique(m$atile[m$matches == 2])

#Part 1
prod(as.numeric(corners)) %>% sprintf('%.0f', .)

#Part 2
get_matches_in_dir <- function(tt){
  revdir <- switch(tt$border,
                   "top" = "bottom",
                   "bottom" = "top",
                   "left" = "right",
                   "right" = "left")
  step <- 0
  matches <- tt %>% select(-matches("UL|UR|LL|LR")) %>% mutate(step = paste0(tt$atile, "_", step))
  all_matches <- bind_rows(all_matches, matches)
  while(nrow(matches) > 0){
    step <- step + 1
    b <- matches$atile
    bdir <- matches$atdir
    bo <- revdir
    
    matches <- m %>% 
      filter(btile == b & btdir == bdir & border == bo) %>% 
      mutate(step = paste0(tt$atile, "_", step))
    
    all_matches <- bind_rows(all_matches, matches)
  }
  
  return(all_matches)
}

carray <- as.data.frame(permutations(4, 4, corners)) %>% 
  rename("UL" = "V1", 
         "UR" = "V2", 
         "LR" = "V3", 
         "LL" = "V4") %>% 
  mutate(id = 1:n())
  

out2 <- pmap_dfr(carray, function(...){
   id <- data.frame(...) %>% pull(id)
  cc <- data.frame(...) %>%
    select(-id) %>%
    pivot_longer(cols = everything(),
                 names_to = "cdir",
                 values_to = "atile")
  mm <- m %>%
    right_join(cc, by = "atile") %>%
    mutate(dirtest = case_when(cdir == "UL" & border %in% c("bottom", "left") ~ TRUE,
                               cdir == "UR" & border %in% c("bottom", "right") ~ TRUE,
                               cdir == "LL" & border %in% c("top", "left") ~ TRUE,
                               cdir == "LR" & border %in% c("top", "right") ~ TRUE,
                               TRUE ~ FALSE)) %>%
    filter(dirtest == TRUE) %>%
    group_by(atile, atdir) %>%
    mutate(matches = n_distinct(border)) %>%
    filter(matches == 2) %>%
    ungroup()

  out <- pmap_dfr(mm, function(...){
    bt <- data.frame(...) %>%
      mutate(opp_end = case_when(cdir == "UL" & border == "bottom" ~ cc$atile[cc$cdir == "UR"],
                                 cdir == "UL" & border == "left" ~ cc$atile[cc$cdir == "LL"],
                                 cdir == "UR" & border == "bottom" ~ cc$atile[cc$cdir == "UL"],
                                 cdir == "UR" & border == "right" ~ cc$atile[cc$cdir == "LR"],
                                 cdir == "LL" & border == "top" ~ cc$atile[cc$cdir == "UL"],
                                 cdir == "LL" & border == "left" ~ cc$atile[cc$cdir == "LR"],
                                 cdir == "LR" & border == "top" ~ cc$atile[cc$cdir == "UR"],
                                 cdir == "LR" & border == "right" ~ cc$atile[cc$cdir == "LL"],
                                 TRUE ~ as.character("NA")))
    
    
    bt2 <- pmap_dfr(bt, function(...) {
      aa <- data.frame(...)
      get_matches_in_dir(aa)
    })
    
    ends <- bt2 %>%
      filter(step == max(step)) %>%
      mutate(opp_end = bt$opp_end)
    
    if(all(ends$atile == ends$opp_end)){
      return(bt2)
    } else {
      return(NULL)
    }
  })
  return(out %>% mutate(id = id))
})


b <- out2 %>% 
  group_by(id) %>% 
  mutate(gs = n()) %>% 
  ungroup() %>% 
  filter(gs == 24)


