

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


# Check Raw Data ####


## Game Data ====

### Check Logical Relations among Variables ----

# Redirect output in console to a text file for processing
sink("Check-Output_Game-Data.txt")

cat("**Logical Relation Violations**\n")
cat("\nQuarter scores not equal to total score, Visitor\n")
game %>% filter(vsco_q1 + vsco_q2 + vsco_q3 + vsco_q4 + vsco_ot
                != vsco_tot) %>% select(2:4, vsco_q1:vsco_tot)
# 4 rows

cat("\nQuarter scores not equal to total score, Host\n")
game %>% filter(hsco_q1 + hsco_q2 + hsco_q3 + hsco_q4 + hsco_ot
                != hsco_tot) %>% select(2:4, hsco_q1:hsco_tot) 
# 18 rows

cat("\nVisitor quarter scores not equal to host quarter scores\n")
game %>% filter(vsco_q1 + vsco_q2 + vsco_q3 + vsco_q4 
                != hsco_q1 + hsco_q2 + hsco_q3 + hsco_q4) %>% select(2:4, vsco_q1:vsco_q4, hsco_q1:hsco_q4) 
# 10 rows

cat("\nVisitor total score equal to host total score\n")
game %>% filter(vsco_tot == hsco_tot) %>% select(2:4, vsco_tot, hsco_tot) 
# 1 row

cat("\nStarters score plus reserves score not equal to total score, Visitor\n")
game %>% filter(vstptsu + vreptsu != vsco_tot) %>% select(2:4, vstptsu, vreptsu, vsco_tot)
# 13 rows

cat("\nStarters score plus reserves score not equal to total score, Host\n")
game %>% filter(hstptsu + hreptsu != hsco_tot) %>% select(2:4, hstptsu, hreptsu, hsco_tot)
# 17 rows

cat("\nNumber of ties in 4q smaller than that in 2m or total number of ties smaller than that in 4q\n")
game %>% filter(tie4q < tie2m | tie < tie4q) %>% select(2:4, tie, tie4q, tie2m)
# 1 row

cat("\nTied duration in 4q smaller than that in 2m or total tied duration smaller than that in 4q\n")
game %>% filter(tiesc4q < tiesc2m | tiesc < tiesc4q) %>% select(2:4, tiesc, tiesc4q, tiesc2m)
# 1 row

cat("\nNumber of lead changes in 4q smaller than that in 2m or total lead changes smaller than that in 4q\n")
game %>% filter(ledchg4q < ledchg2m | ledchg < ledchg4q) %>% select(2:4, ledchg, ledchg4q, ledchg2m)
# 1 row

cat("\nHost lead duraion in 4q smaller than that in 2m or total host lead duration smaller than that in 4q\n")
game %>% filter(hledsc4q < hledsc2m | hledsc < hledsc4q) %>% select(2:4, hledsc, hledsc4q, hledsc2m)
# 2 rows

cat("\nNumber of games played smaller than number of wins, Visitor\n")
game %>% filter(vgp < vgpw) %>% select(2:4, vgp, vgpw)
# 3 rows

cat("\nNumber of games played smaller than number of wins, Host\n")
game %>% filter(hgp < hgpw) %>% select(2:4, hgp, hgpw)
# 1 row

cat("\nNumber of games played smaller than absolute value of streak, Visitor\n")
game %>% filter(vgp < abs(vstrk)) %>% select(2:4, vstrk, vgp)
# 0 rows

cat("\nNumber of games played smaller than absolute value of streak, Host\n")
game %>% filter(hgp < abs(hstrk)) %>% select(2:4, hstrk, hgp)
# 0 rows

cat("\nTotal game minutes not equal to minutes played by starters and reserves, Visitor\n")
cat("\nNote: 'vmpsu' does not exist in original data\n")
game %>% mutate(vmpsu = round(vstmpsu + vrempsu, 0)) %>%
  filter(abs(vmpsu - (48 + 5*otnum)*5) > 1) %>%
  select(2:4, vstmpsu, vrempsu, vmpsu, otnum) %>% 
  arrange(otnum, vmpsu)
# 13 rows

cat("\nTotal game minutes not equal to minutes played by starters and reserves, Host\n")
cat("\nNote: 'hmpsu' does not exist in original data\n")
game %>% mutate(hmpsu = round(hstmpsu + hrempsu, 0)) %>%
  filter(abs(hmpsu - (48 + 5*otnum)*5) > 1) %>%
  select(2:4, hstmpsu, hrempsu, hmpsu, otnum) %>% 
  arrange(otnum, hmpsu)
# 19 rows

cat("\nTotal seconds not equal to sum of tied, visitor lead and host lead seconds\n")
cat("\nNote: 'totsc' does not exist in original data\n")
game %>% mutate(totsc = round(tiesc + vledsc + hledsc, 0)) %>% 
  filter(abs(totsc - (2880 + 300*otnum)) > 18) %>% 
  select(2:4, tiesc, vledsc, hledsc, totsc, otnum) %>% 
  arrange(otnum, totsc) %>% print(n = Inf)
# 33 rows

# Pause redirecting console output
sink()

### Check NAs and Outliers ----

# Check NAs
colSums(is.na(game))[colSums(is.na(game)) > 0]
# no column with incidental NAs

# Create function 'abn' to return the smallest and largest n values in each
# column in Game data
abn <- function(x, n) {
  a <- sort(x, na.last = NA)[1:n]
  b <- sort(x, na.last = NA, decreasing = TRUE)[1:n]
  c(a, rev(b))
}

# Create a data frame to inspect potential outliers
check1 <- as.data.frame(lapply(game, abn, n = 10))

# Return outliers values identified above using function 'fd'
fd <- function(col_name, n1, n2) {
  game[game[[col_name]] >= n1 & game[[col_name]] <= n2, 
       c("date", "visitor", "host", col_name)]
}

# Continue redirecting output in console to a text file for processing 
sink("Check-Output_Game-Data.txt", append = TRUE)

cat("\n**Outliers (by last column)**\n")
cat("\n")
fd("vapp", 5, 5)
cat("\n")
fd("vstmpsu", 20, 20)
cat("\n")
fd("htovpc", 113.2, 113.2)
cat("\n")
fd("hefgpc", 1, 1)
cat("\n")
fd("hsco_q2", 223, 223)
cat("\n")
fd("hsco_q3", 119, 119)
cat("\n")
fd("hgpw", 104, 104)
cat("\n")
fd("vgpw", 119, 119)
cat("\n")
fd("lsp3", 2, 2)
cat("\n")
fd("hstptsu", 887, 887)
cat("\n")
fd("happ", 99, 99)
cat("\n")
fd("vpmav", 20, 20)
cat("\n")
fd("vpmav", -20, -20)
cat("\n")
fd("vstptsu", 977, 977)
cat("\n")
fd("vsco_q1",2,2)
cat("\n")
fd("hsco_q4",2,2) 
cat("\n")
fd("vrempsu",22,22)
cat("\n")
fd("vrempsu",23,23)
cat("\n")
fd("hstmpsu",24,24)
cat("\n")
fd("hrempsu",24,24)
cat("\n")
fd("hrempsu",187,187)
cat("\n")

# Stop redirecting console output
sink()


## Season Data ====


### Check NAs and Outliers ----

# Check NAs
colSums(is.na(season))[colSums(is.na(season)) > 0]
# 2 variables that have NAs： ‘attsn', 'attpgsn'

# Create a data frame to inspect potential outliers using function 'abn'
check2 <- as.data.frame(lapply(season, abn, n = 10))

# Return outliers values identified above using function 'fd2'
fd2 <- function(col_name, n1, n2) {
  season[season[[col_name]] >= n1 & season[[col_name]] <= n2, 
         c("season", "team", col_name)]
}

# Redirect output in console to a text file for processing
sink("Check-Output_Season-Data.txt")

cat("**Rows with NAs**\n")
cat("\n")
season %>% filter(is.na(attsn) | is.na(attpgsn)) %>% 
  select(1, 3, attsn, attpgsn)

cat("\n**Outliers (by last column)**\n")
cat("\n")
fd2("uspcsn", 1.0625, 1.0625)
cat("\n")
fd2("uspcsn", 0, 0.05882353)
cat("\n")
fd2("expavsn",8.05,8.07)
cat("\n")
fd2("expavsn",8.82,8.84)
cat("\n")

# Stop redirecting console output
sink()
