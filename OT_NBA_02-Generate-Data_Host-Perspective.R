

# Load Initial Data ####


library(tidyverse)

game <- read_rds("Initial-Data_Game.rds")
season <- read_rds("Initial-Data_Season.rds")


# Generate Data (Host Perspective) ####


## Process Game Data ====

game <- game %>%
  mutate(
    scodiff = hsco_tot - vsco_tot,
    hwin = case_when(scodiff > 0 ~ 1, scodiff < 0 ~ 0)) %>%
  rename(hforced = lsv) %>%
  select(season, date, visitor, host, scodiff, hwin, hforced, everything())

## Join Game Data with Season Data ====

# Rename variables in Season data to prepare join with Game data for host
# teams and visitor teams
hseason <- season
vseason <- season

colnames(hseason) <- paste("h", colnames(hseason), sep = "_")
colnames(vseason) <- paste("v", colnames(vseason), sep = "_")

data <- game %>%
  # join Season data for host teams
  left_join(hseason,
            by = c("season" = "h_season",
                   "host" = "h_team")) %>%
  # join Season data for visitor teams
  left_join(vseason,
            by = c("season" = "v_season",
                   "visitor" = "v_team"))


# Save Data (Host Perspective) ####


write_rds(data, "Data_HP.rds")

# haven::write_dta(data, "Data_HP.dta")

# readr::write_csv(data, "Data_HP.csv")

