
library(tidyverse)

# Load data
game <- read_rds("Initial-Data_Game.rds")
season <- read_rds("Initial-Data_Season.rds")

# Generate Data (Host Perspective) ####

## Process Data ====

# Create and rename variables in Game data
game <- game %>%
  mutate(
    scodiff = hsco_tot - vsco_tot,
    hwin = case_when(scodiff > 0 ~ 1, scodiff < 0 ~ 0)) %>%
  rename(hforced = lsv) %>%
  select(season, date, visitor, host, scodiff, hwin, hforced, everything())

# Duplicate Season data for host and visitor teams to join with Game data
hseason <- season
vseason <- season

colnames(hseason) <- paste("h", colnames(hseason), sep = "_")
colnames(vseason) <- paste("v", colnames(vseason), sep = "_")

# Join Game data with Season data
data <- game %>%
  # join Season data for Host teams
  left_join(hseason,
            by = c("season" = "h_season",
                   "host" = "h_team")) %>%
  # join Season data for Visitor teams
  left_join(vseason,
            by = c("season" = "v_season",
                   "visitor" = "v_team"))

## Factor Variables ====

data$hwin <- factor(data$hwin,
                    levels = c(0, 1),
                    labels = c("Host Lost", "Host Won"))

data$hforced <- factor(data$hforced,
                       levels = c(0, 1),
                       labels = c("Host Forcing OT1", "Host Forced to OT1"))

data$type <- factor(data$type,
                    levels = c(0:4),
                    labels = c("Regular Games", "Conference 1st Round",
                               "Conference Semifinals", "Conference Finals",
                               "Finals"))

data$lsp3 <- factor(data$lsp3,
                    levels = c(0, 1),
                    labels = c("Last Goal Not 3-Pointer",
                               "Last Goal Was 3-Pointer"))

data$h_east <- factor(data$h_east,
                      levels = c(0, 1),
                      labels = c("Western", "Eastern"))

data$v_east <- factor(data$v_east,
                      levels = c(0, 1),
                      labels = c("Western", "Eastern"))

data$h_levelsn <- factor(data$h_levelsn,
                         levels = c(0:5),
                         labels = c("No Playoff", "Conference 1st Round",
                                    "Conference Semifinals", "Conference Finals",
                                    "Finals", "Champions"))

data$v_levelsn <- factor(data$v_levelsn,
                         levels = c(0:5),
                         labels = c("No Playoff", "Conference 1st Round",
                                    "Conference Semifinals", "Conference Finals",
                                    "Finals", "Champions"))

# Save Data (Host Perspective) ####

write_rds(data, "Data_HP.rds")