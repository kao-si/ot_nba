
# Process Raw Data ####

library(tidyverse)
library(readxl)

## Load Data from Excel ====

game_0211 <- read_excel("Raw-Data/Data_Game_2002-2011.xlsx")
game_1221 <- read_excel("Raw-Data/Data_Game_2012-2021.xlsx")
season_0211 <- read_excel("Raw-Data/Data_Season_2002-2011.xlsx")
season_1221 <- read_excel("Raw-Data/Data_Season_2012-2021.xlsx")

## Combine Data ====

game <- bind_rows(game_0211, game_1221)
season <- bind_rows(season_0211, season_1221)

## Tidy Season Data ====

# Set team abbreviations to lower case letters
season$team <- str_to_lower(season$team)

# Construct certain percentage variables
season <- season %>%
  separate(howpcsn, c("howsn", "holsn"), convert = TRUE) %>%
  separate(rowpcsn, c("rowsn", "rolsn"), convert = TRUE) %>%
  separate(ewpcsn, c("ewsn", "elsn"), convert = TRUE) %>%
  separate(wwpcsn, c("wwsn", "wlsn"), convert = TRUE) %>%
  separate(f3wpcsn, c("f3wsn", "f3lsn"), convert = TRUE) %>%
  separate(m10wpcsn, c("m10wsn", "m10lsn"), convert = TRUE) %>%
  mutate(
    howpcsn = howsn/(howsn + holsn),
    rowpcsn = rowsn/(rowsn + rolsn),
    ewpcsn = ewsn/(ewsn + elsn),
    wwpcsn = wwsn/(wwsn + wlsn),
    f3wpcsn = f3wsn/(f3wsn + f3lsn),
    m10wpcsn = m10wsn/(m10wsn + m10lsn)
  )

## Reconcile Team Abbreviations between Game and Season Data ====

# Some team abbreviations were not exactly the same in Game data vs. in Season
# data during data collection and therefore need to be reconciled

# Extract all team abbreviations in Game data
gkey <- game %>% 
  select(season, date, visitor, host) %>%
  gather(visitor, host, key = "role", value = "team")

# Anti-join with Season data to find rows that need to be reconciled
check_abbr <- anti_join(gkey, season, by = c("season", "team"))

# Identify team abbreviations and seasons that need to be reconciled; also
# check that there is no difference by role (host vs. visitor)
table(check_abbr$season, check_abbr$role, check_abbr$team)

# Team abbreviations that need to be reconciled:

# | Abbr in Game data | Seasons involved |
# | ----------------- | ---------------- |
# | 'brk'             | 201213-202122    |
# | 'cho'             | 201415-202122    |
# | 'noh'             | 200708-201011    |
# | 'pho'             | 200203-202122    |



