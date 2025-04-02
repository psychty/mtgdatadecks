
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
library_df <- read_csv(paste0(card_store, '/card_library.csv'))

# Decks (this might be decks you want or have) ####
temur_deck_df <- read.delim(paste0(deck_store, "temur-roar-tarkir-dragonstorm-commander-precon-decklist-20250326-003635.txt"), 
         header = FALSE) %>% 
  mutate(quantity = as.numeric(str_before_first(V1, ' '))) %>% 
  mutate(name = str_after_first(V1, ' ')) %>% 
  select(!V1)

drana_deck_df <- read.delim(paste0(deck_store, "/back-to-life-back-to-reality--20231115-153612.txt"), 
                            header = FALSE) %>% 
  mutate(quantity = as.numeric(str_before_first(V1, ' '))) %>% 
  mutate(name = str_after_first(V1, ' ')) %>% 
  select(!V1)
  
# Purchases ####
purchases <- read_sheet('https://docs.google.com/spreadsheets/d/1-Q8ZliqPYD0RGHXeL2FIvZoSIeDOg29rfbC-O5Hqw8k/edit?usp=sharing') 
2

purchases <- purchases %>% 
  bind_rows(temur_deck_df) # include this (and any other decks) because it is a bought deck
  
# Add metadata for new cards from scryfall
cards_to_search_df <- purchases %>% 
  mutate(name = gsub("\\'", "", name)) %>% 
  filter(!name %in% library_df$sanitised_name) %>% 
  filter(!name %in% c('Ooze', 'Treasure', 'Morph', 'Plains', 'Mountain', 'Swamp', 'Forest', 'Island')) # Stop trying to search for basic lands or tokens

# Only run this if cards to search is greater than zero (e.g. if there are codes to search_)
if(nrow(cards_to_search_df) > 0){

# Using our dataframe
for(i in 1:nrow(cards_to_search_df)){

if(i == 1){
card_meta <- data.frame()
}
  
name_x <- cards_to_search_df$name[i]

card_x_meta <- scry_cards(paste0('name:',name_x)) 

card_meta <- card_meta %>% 
  bind_rows(card_x_meta) 

}

# Update meta df ####

  
card_meta_new <- data.frame(name = character(), mana_cost = character(), cmc = numeric(), oracle_text = character(), power = numeric(), toughness = numeric(), legalities = list(), type_line = character(), keywords = list(), layout = character(), prints_search_uri = character(), rarity = character())  %>% 
  bind_rows(card_meta) %>% 
  select(name, mana_cost, combined_mana_cost = cmc, card_text = oracle_text, power, toughness, legalities, type = type_line, keywords, layout, prints_search_uri, rarity) %>% 
  unnest(legalities) %>% 
  select(!c(future, historic, timeless, gladiator, pioneer, explorer, modern, legacy, pauper, vintage, penny, oathbreaker, standardbrawl, brawl, alchemy, paupercommander, duel, oldschool, premodern, predh)) %>% 
  mutate(keywords = as.character(keywords)) %>% 
  mutate(keywords = case_when(keywords == 'NULL' ~ NA,
                              TRUE ~ trimws(keywords))) %>% 
  mutate(number_keywords = str_count(keywords, '\\,') + 1) %>% 
  mutate(keywords = gsub('^c\\(|\\)$|\\"', '', keywords)) %>% 
  mutate(Flying = case_when(str_detect(keywords, 'Flying') ~ 'Flying',
                            TRUE ~ 'Not flying'),
         Deathtouch = case_when(str_detect(keywords, 'Deathtouch') ~ 'Deathtouch',
                                 TRUE ~ 'Not deathtouch'),
         Haste = case_when(str_detect(keywords, 'Haste') ~ 'Haste',
                            TRUE ~ 'Not haste'),
         Vigilance =  case_when(str_detect(keywords, 'Vigilance') ~ 'Vigilance',
                                TRUE ~ 'Not vigilance')) %>% 
  separate_wider_delim(keywords,
                       delim = ",",
                       names = c("First_keyword", "Second_keyword", "Third_keyword", 'Fourth_keyword', 'Fifth_keyword', 'Sixth_keyword'),
                       too_few = "align_start") %>% 
  mutate(Broad_type = case_when(
    str_detect(type, '[Cc]reature') ~ 'Creature',
    str_detect(type, 'Planeswalker') ~ 'Planeswalker',
    str_detect(type, 'Artifact') ~ 'Artifact',
    str_detect(type, 'Land') ~ 'Land',
    !str_detect(type, ' — ') ~ type,
    TRUE ~ str_before_first(type, ' — '))) %>% 
  unique %>% 
  mutate(sanitised_name = case_when(
    name == "Trostani's Judgment" ~ 'Trostanis Judgment',
    name == "Coursers' Accord" ~ 'Coursers Accord',
    name == "Assassin's Strike" ~ 'Assassins Strike',
    name == "Bane's Invoker" ~ 'Banes Invoker',
    name == "+2 Mace" ~ 'Mace',
    name == "Kinjalli's Dawnrunner" ~ 'Kinjallis Dawnrunner',
    name == "Citizen's Crowbar" ~ 'Citizens Crowbar',
    name == "Urza's Rage" ~ 'Urzas Rage',
    name == "Gimli's Fury" ~ 'Gimlis Fury',
    name == "Light 'Em Up" ~ 'Light Em Up',
    name == "Healer's Hawk" ~ 'Healers Hawk',
    name == "Bandit's Talent" ~ 'Bandits Talent',
    name == "Kolaghan's Command" ~ 'Kolaghans Command',
    name == "Seer's Sundial" ~ 'Seers Sundial',
    name == "Séance" ~ 'Seance',
    name == 'Stormshriek Feral // Flush Out' ~ 'Stormshriek Feral',
    name == 'Whirlwing Stormbrood // Dynamic Soar' ~ 'Whirlwing Stormbrood',
    TRUE ~ gsub("\\'", '', name))) %>% 
  filter(sanitised_name %in% cards_to_search_df$name) %>% 
  mutate(toughness = as.numeric(toughness)) %>% 
  mutate(power = as.numeric(power)) 

cards_not_found <- cards_to_search_df %>% 
  filter(!name %in% card_meta_new$sanitised_name) 

if(nrow(card_meta_new) >0){

  
# update card meta
  
# Append new card metadata to old card metadata  
  read_csv(paste0(card_store, '/card_meta.csv')) %>% 
  bind_rows(card_meta_new) %>%
  unique() %>% 
  write.csv(., paste0(card_store, '/card_meta.csv'), row.names = FALSE)
  
# update card library  
  purchases %>%
    select(quantity, name) %>%
  filter(!name %in% c('Plains','Island', 'Swamp', 'Forest', 'Mountain')) %>% # remove basic lands
  filter(!name %in% c('Morph', 'Ooze', 'Treasure')) %>% # remove tokens
  mutate(name = case_when(
    name == 'Mace' ~ '+2 Mace',
    name == "Racer's Ring" ~ "Racers' Ring",
    name == 'Seance' ~ 'Séance',
    name == 'Stormshriek Feral' ~ 'Stormshriek Feral // Flush Out',
    name == 'Whirlwing Stormbrood' ~ 'Whirlwing Stormbrood // Dynamic Soar',
    name == 'Katildas Rising Dawn' ~ "Katilda, Dawnhart Martyr // Katilda's Rising Dawn",
    name == 'Healers Hawk' ~ "Healer's Hawk",
    TRUE ~ name) )%>%
  group_by(name) %>%
  summarise(quantity = sum(quantity)) %>%
  left_join(read_csv(paste0(card_store, '/card_meta.csv')), by = 'name') %>% 
    write.csv(.,
              paste0(card_store, '/card_library.csv'),
              row.names = FALSE)
    
  # Export current library as text to put into price search
  read_csv(paste0(card_store, '/card_library.csv')) %>% 
    select(quantity, name) %>% 
    write.table(., 
                file = paste0(card_store, '/card_library.txt'),
                row.names = FALSE,
                col.names = FALSE,
                quote = FALSE,
                sep = ' ')

}

rm(card_meta, card_x_meta)

}

if(nrow(cards_to_search_df) == 0) {
rm(cards_to_search_df)
}

meta_df <- read_csv(paste0(card_store, '/card_meta.csv')) %>% 
  unique()

library_df <- read_csv(paste0(card_store, '/card_library.csv'))

# Banned ####
library_df %>% 
  filter(commander == 'banned')

# Check any of library on banned list 
# scry_cards("legalities ")
# banned:

# https://curso-r.github.io/scryr/articles/syntax.html

# scry_bulk_file(toString(deck_df$name))

# Scryfall database using scryr package to interface with the api

# Find red creatures with 7 power, sorted by converted mana cost
# cards <- scry_cards("c:red+pow=7", order = "cmc")

# Legendary vampires
# vampires <- scry_cards("t:vampire t:legend")

# There is a useful autocomplete function if you're unsure of the full name of a card
autocomplete_name("apocalypse")[12]

# Wants ####

tobuy_df <- read_sheet('https://docs.google.com/spreadsheets/d/1qYWPSF9s3zLrmtn2uH-dh-6CnXbe_sy2deGWfCYDvpU/edit?usp=sharing')

tobuy_df %>% 
  filter(name %in% library_df$name)


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

