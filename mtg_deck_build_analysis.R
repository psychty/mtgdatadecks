
# This script is for deck building and visualising  

# Loading some packages 
packages <- c('easypackages','readxl', 'tidyr', 'dplyr', 'readr', 'jsonlite', 'httr', 'rvest', 'stringr', 'scales', 'ggplot2','sjmisc', 'beepr', 'strex', 'keyring', 'scryr', 'googlesheets4')
install.packages(setdiff(packages, rownames(installed.packages())))
easypackages::libraries(packages)
local_store <- '~/Repositories/mtgdatadecks'

deck_store <- paste0(local_store, '/decklists/')
card_store <- paste0(local_store, '/cards/')

mtg_mana_colours <- data.frame(mana_type = c('Forest', 'Island', 'Mountain', 'Plain', 'Swamp'),
                               mana_colour = c('Green', 'Blue', 'Red', 'White', 'Black'),
                               light_colour =  c('#c4d3ca', '#b3ceea', '#eb9f82', '#f8e7b9', '#a69f9d'),
                               dark_colour = c('#00733e', '#0e67ab', '#d3202a', '#f9faf4', '#150b00'))

# Load card library ####
library_df <- read_csv(paste0(card_store, '/card_library.csv')) %>% 
  mutate(card_text = gsub('\r', '',  gsub('\n', '', card_text))) %>% 
  unique()

are_you_orcay <- read_sheet('https://docs.google.com/spreadsheets/d/1R9vn1k23P521dqHaXR00pqMGr_pH1JYwT4o04uR-6-E/edit?usp=sharing') 
2

are_you_orcay <- are_you_orcay %>% 
  left_join(library_df %>% select(!quantity), by = 'name')

are_you_orcay %>% 
  select(quantity, name) %>% 
  unique() %>% 
  write.table(., 
              file = paste0(deck_store, 'are_you_orcay_hun_deck.txt'),
              row.names = FALSE,
              col.names = FALSE,
              quote = FALSE,
              sep = ' ')

drive_upload(
  paste0(deck_store, 'are_you_orcay_hun_deck.txt'),
  overwrite = TRUE)



library_df %>% 
  mutate(Flying = case_when(First_keyword == 'Flying' | Second_keyword == 'Flying' ~ 'Flying',
                            TRUE ~ 'Not flying'))








# Tribe identity
# Probably want to create a big ish list of tribes as well as an other (e.g. pirates, humans, merfolk etc)
library_df %>% 
  filter(Broad_type == 'Creature') %>% 
  pull(name) %>% unique



temur_deck_df <- read.delim(paste0(deck_store, "temur-roar-tarkir-dragonstorm-commander-precon-decklist-20250326-003635.txt"), 
                            header = FALSE) %>% 
  mutate(quantity = as.numeric(str_before_first(V1, ' '))) %>% 
  mutate(name = str_after_first(V1, ' ')) %>% 
  select(!V1) 

temur_meta <- library_df %>% 
  filter(name %in% temur_deck_df$name)


# Banned ####
library_df %>% 
  filter(commander == 'banned') %>% 
  pull(name)

# to find


# Wants ####

tobuy_df <- read_sheet('https://docs.google.com/spreadsheets/d/1qYWPSF9s3zLrmtn2uH-dh-6CnXbe_sy2deGWfCYDvpU/edit?usp=sharing')
# 2
# 
# tobuy_df %>% 
#   filter(name %in% library_df$name)

library_df %>% 
  group_by(combined_mana_cost) %>% 
  summarise(Cards = n()) %>% 
  ggplot() +
  geom_bar(aes(x = combined_mana_cost,
               y = Cards),
           stat = 'identity')

library_df %>% 
  group_by(Broad_type) %>% 
  summarise(Cards = n()) %>% 
  ggplot() +
  geom_bar(aes(x = Broad_type,
               y = Cards),
           stat = 'identity')

# Plot mana curves
# Odds of drawing land
# Odds of drawing x card
# Which colour type - icons showing number of cards
# Colour combo name (e.g. Bant for White Green Blue)
# What are you hoping for
# How much flying, reach
# Removal, Tokens, Go wide, Counter spells etc.

# avg combined mana cost
# how much interacting, control, aggro key words in here

#https://draftsim.com/mtg-edh-deck-builder/

# https://www.mtgotraders.com/articles/notlosemana.html#:~:text=The%20basic%20rule%20of%20thumb,in%20a%2060%20card%20deck.

# Create a sample deck
deck <- c("Mountain", "Mountain", "Black Lotus", "Channel", "Fireball")
# Draw a hand
hand <- sample(deck, 5)
print(hand)

