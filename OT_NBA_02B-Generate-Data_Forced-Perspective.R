
library(tidyverse)

# Load data
game_initial <- read_rds("Initial-Data_Game.rds")
season <- read_rds("Initial-Data_Season.rds")

# Generate Data (Forced Perspective) ####

## Process Data ====

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

# Replace first letter 'h' with 't' and replace first letter 'v' with 'f' in
# variable names in 'lsv0'
lsv0 <- lsv0 %>% rename(fteam = visitor, tteam = host) %>%
  rename_with(~str_replace(., "^h", "t")) %>%
  rename_with(~str_replace(., "^v", "f"))

# Replace first letter 'v' with 't' and replace first letter 'h' with 'f' in
# variable names in 'lsv1'
lsv1 <- lsv1 %>% rename(tteam = visitor, fteam = host) %>%
  rename_with(~str_replace(., "^v", "t")) %>%
  rename_with(~str_replace(., "^h", "f"))

# Bind 'lsv0' and 'lsv1' to generate Game data in Forced Perspective
game <- bind_rows(lsv0, lsv1) %>%
  rename(fhost = lsv)

# Create and rename variables
game <- game %>% mutate(
  scodiff = fsco_tot - tsco_tot,
  fwin = case_when(scodiff > 0 ~ 1, scodiff < 0 ~ 0)) %>%
  select(season, date, tteam, fteam, scodiff, fwin, fhost, everything())

# Duplicate Season data for forced and trailing teams to join with Game data
fseason <- season
tseason <- season

colnames(fseason) <- paste("fteam", colnames(fseason), sep = "_")
colnames(tseason) <- paste("tteam", colnames(tseason), sep = "_")

# Join Game data with Season data
data <- game %>%
  # join Season data for Forced teams
  left_join(fseason,
            by = c("season" = "fteam_season",
                   "fteam" = "fteam_team")) %>%
  # join Season data for Trailing teams
  left_join(tseason,
            by = c("season" = "tteam_season",
                   "tteam" = "tteam_team"))

## Factor Variables ====

data$fwin <- factor(data$fwin,
                    levels = c(0, 1),
                    labels = c("Forced Team Lost", "Forced Team Won"))

data$fhost <- factor(data$fhost,
                       levels = c(0, 1),
                       labels = c("Forced Team Was Visitor",
                                  "Forced Team Was Host"))

data$type <- factor(data$type,
                    levels = c(0:4),
                    labels = c("Regular Games", "Conference 1st Round",
                               "Conference Semifinals", "Conference Finals",
                               "Finals"))

data$lsp3 <- factor(data$lsp3,
                    levels = c(0, 1),
                    labels = c("Last Goal Not 3-Pointer",
                               "Last Goal Was 3-Pointer"))

data$fteam_east <- factor(data$fteam_east,
                      levels = c(0, 1),
                      labels = c("Western", "Eastern"))

data$tteam_east <- factor(data$tteam_east,
                      levels = c(0, 1),
                      labels = c("Western", "Eastern"))

data$fteam_levelsn <- factor(data$fteam_levelsn,
                         levels = c(0:5),
                         labels = c("No Playoff", "Conference 1st Round",
                                    "Conference Semifinals", "Conference Finals",
                                    "Finals", "Champions"))

data$tteam_levelsn <- factor(data$tteam_levelsn,
                         levels = c(0:5),
                         labels = c("No Playoff", "Conference 1st Round",
                                    "Conference Semifinals", "Conference Finals",
                                    "Finals", "Champions"))

# Save Data (Forced Perspective) ####

write_rds(data, "Data_FP.rds")