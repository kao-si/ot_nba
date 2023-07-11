

# Load Initial Data ####


library(tidyverse)

game_initial <- read_rds("Initial-Data_Game.rds")
season <- read_rds("Initial-Data_Season.rds")


# Generate Data (Forced Perspective) ####


## Process Game Data ====

# 'lsv0': games in which the host team forced the 1st OT, in which case
# 'Forced' team is the Visitor team and 'Trailing' team is the Host team;

# 'lsv1': games in which the visitor team forced the 1st OT, in which case
# 'Forced' team is the Host team and 'Trailing' team is the Visitor team.
lsv0 <- filter(game_initial, lsv == 0)
lsv1 <- filter(game_initial, lsv == 1)

# Change 'H' perspective variables to 'F' perspective variables in 'lsv0'
lsv0 <- lsv0 %>% mutate(
  fledsc4q = 720 - tiesc4q - hledsc4q,
  fled2m = - hled2m,
  fledsc2m = 120 - tiesc2m - hledsc2m,
  hledsc4q = NULL,
  hled2m = NULL,
  hledsc2m = NULL
)

# Replace first letter 'h' with 't' and 'v' with 'f' in variable names in 'lsv0'
lsv0 <- lsv0 %>% rename(teamf = visitor, teamt = host) %>%
  rename_with(~str_replace(., "^h", "t")) %>%
  rename_with(~str_replace(., "^v", "f"))

# Replace first letter 'v' with 't' and 'h' with 'f' in variable names in 'lsv1'
lsv1 <- lsv1 %>% rename(teamt = visitor, teamf = host) %>%
  rename_with(~str_replace(., "^v", "t")) %>%
  rename_with(~str_replace(., "^h", "f"))

# Bind 'lsv0' and 'lsv1' to generate Game data in Forced Perspective
game <- bind_rows(lsv0, lsv1) %>%
  rename(fhost = lsv)

# Create and rename variables
game <- game %>% mutate(
  scodiff = fsco_tot - tsco_tot,
  fwin = case_when(scodiff > 0 ~ 1, scodiff < 0 ~ 0)) %>%
  select(season, date, teamt, teamf, scodiff, fwin, fhost, everything())

## Join Game Data with Season Data ====

# Rename variables in Season data to prepare join with Game data for Forced
# teams and Trailing teams
fseason <- season
tseason <- season

colnames(fseason) <- paste("teamf", colnames(fseason), sep = "_")
colnames(tseason) <- paste("teamt", colnames(tseason), sep = "_")

data <- game %>%
  # join Season data for Forced teams
  left_join(fseason,
            by = c("season" = "teamf_season",
                   "teamf" = "teamf_team")) %>%
  # join Season data for Trailing teams
  left_join(tseason,
            by = c("season" = "teamt_season",
                   "teamt" = "teamt_team"))


# Save Data (Forced Perspective) ####


# write_rds(data, "Data_FP.rds")

# haven::write_dta(data, "Data_FP.dta")

# readr::write_csv(data, "Data_FP.csv")

