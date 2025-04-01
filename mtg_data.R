
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

# Library ####

temur_deck_df <- read.delim("mtgdatadecks/decklists/temur-roar-tarkir-dragonstorm-commander-precon-decklist-20250326-003635.txt", 
         header = FALSE) %>% 
  mutate(quantity = as.numeric(str_before_first(V1, ' '))) %>% 
  mutate(name = str_after_first(V1, ' ')) %>% 
  select(!V1)

drana_deck_df <- read.delim("~/Repositories/mtgdatadecks/decklists/back-to-life-back-to-reality--20231115-153612.txt", 
                            header = FALSE) %>% 
  mutate(quantity = as.numeric(str_before_first(V1, ' '))) %>% 
  mutate(name = str_after_first(V1, ' ')) %>% 
  select(!V1)

# Which cards do I already have information on ####
library_df <- read_csv(paste0(local_store, '/card_meta.csv'))

whatnotbuys <- read_sheet('https://docs.google.com/spreadsheets/d/1-Q8ZliqPYD0RGHXeL2FIvZoSIeDOg29rfbC-O5Hqw8k/edit?usp=sharing')
2

# Additional data from scryfall ####
cards_to_search_df <- whatnotbuys %>% 
  bind_rows(temur_deck_df) %>% 
  bind_rows(drana_deck_df) %>% 
  mutate(name = gsub("\\'", "", name)) %>% 
  filter(!name %in% library_df$sanitised_name)

# https://curso-r.github.io/scryr/articles/syntax.html

# scry_bulk_file(toString(deck_df$name))

# Scryfall database using scryr package to interface with the api

# Find red creatures with 7 power, sorted by converted mana cost
# cards <- scry_cards("c:red+pow=7", order = "cmc")

# Legendary vampires
# vampires <- scry_cards("t:vampire t:legend")


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

# Update library df ####

card_meta_new <- card_meta %>% 
  select(name, mana_cost, combined_mana_cost = cmc, card_text = oracle_text, power, toughness, legalities, type = type_line, keywords, layout, arena_id, prints_search_uri, rarity) %>% 
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
                       names = c("First_keyword", "Second_keyword", "Third_keyword", 'Fourth_keyword', 'Fifth_keyword'),
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
    name == "Bandit's Talent" ~ 'Bandits Talent',
    name == "Kolaghan's Command" ~ 'Kolaghans Command',
    name == "Seer's Sundial" ~ 'Seers Sundial',
    name == "Séance" ~ 'Seance',
    name == 'Stormshriek Feral // Flush Out' ~ 'Stormshriek Feral',
    name == 'Whirlwing Stormbrood // Dynamic Soar' ~ 'Whirlwing Stormbrood',
    TRUE ~ gsub("\\'", '', name))) %>% 
  filter(sanitised_name %in% cards_to_search_df$name) %>% 
  mutate(toughness = as.numeric(toughness))

cards_not_found <- cards_to_search_df %>% 
  filter(!name %in% card_meta_new$sanitised_name) 

if(nrow(card_meta_new) >0){

library_df %>%
  bind_rows(card_meta_new) %>%
  unique() %>% 
  write.csv(., paste0(local_store, '/card_meta.csv'), row.names = FALSE)

}

library_df <- read_csv(paste0(local_store, '/card_meta.csv')) %>% 
  unique()

# Banned ####

library_df %>% 
  filter(commander == 'banned')

# Check any of library on banned list 
# scry_cards("legalities ")
# banned:

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

