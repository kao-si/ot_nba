

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


# Modify Raw Data and Generate Initial Data ####


## Modify team abbreviations in Season data ====

season$team[season$team == "bkn"] <- "brk"

season$team[season$team == "cha" & season$season >= 201415] <- "cho"

season$team[season$team == "nop" & season$season >= 200708 
            & season$season <= 201011] <- "noh"

season$team[season$team == "phx"] <- "pho"

## Modify Game data using function 'md' ====

md <- function(datev, vname, hname, col_name, newval) {
  game[[col_name]][game$date == datev &
                  game$visitor == vname &
                  game$host == hname] <- newval
  return(game)
}

game <- md(20090429, "dal", "mem", "date", 20060429)
game <- md(20090430, "pho", "lal", "date", 20060430)

game <- md(20041105, "njn", "chi", "otnum", 2)
game <- md(20041206, "cha", "lac", "otnum", 2)

game <- md(20170121, "sas", "cle", "vsco_q1", 22)
game <- md(20100113, "sas", "okc", "vsco_q3", 13)

game <- md(20030212, "chi", "phi", "vsco_ot", 2)
game <- md(20140416, "uta", "min", "vsco_ot", 29)

game <- md(20070128, "sas", "lal", "hsco_q1", 16)
game <- md(20070307, "cha", "pho", "hsco_q1", 30)
game <- md(20050128, "phi", "noh", "hsco_q2", 23)
game <- md(20100221, "sas", "det", "hsco_q2", 21)
game <- md(20021104, "chi", "tor", "hsco_q3", 25)
game <- md(20120408, "chi", "nyk", "hsco_q3", 19)
game <- md(20021104, "chi", "tor", "hsco_q4", 14)
game <- md(20051102, "lal", "den", "hsco_q4", 22)
game <- md(20060110, "pho", "den", "hsco_q4", 27)

game <- md(20050309, "njn", "noh", "hsco_ot", 8)
game <- md(20051219, "det", "mem", "hsco_ot", 17)
game <- md(20061101, "nyk", "mem", "hsco_ot", 28)
game <- md(20070119, "det", "min", "hsco_ot", 19)
game <- md(20080201, "nyk", "por", "hsco_ot", 13)
game <- md(20080206, "noh", "pho", "hsco_ot", 19)

game <- md(20100323, "cha", "was", "hsco_tot", 86)
game <- md(20100402, "mil", "cha", "hsco_tot", 87)
game <- md(20110316, "orl", "mil", "hsco_tot", 89)
game <- md(20190322, "mem", "orl", "hsco_tot", 123)

game <- md(20170402, "ind", "cle", "hefgpc", 0.589)

game <- md(20091208, "cle", "mem", "htovpc", 13.2)

game <- md(20130417, "hou", "lal", "vapp", 9)

game <- md(20030211, "sas", "por", "vstmpsu", 201)
game <- md(20030212, "chi", "phi", "vstmpsu", 164)
game <- md(20050311, "atl", "tor", "vstmpsu", 186)
game <- md(20080324, "pho", "det", "vstmpsu", 198)
game <- md(20110119, "mem", "noh", "vstmpsu", 198)

game <- md(20030211, "sas", "por", "vrempsu", 64)
game <- md(20030212, "chi", "phi", "vrempsu", 101)
game <- md(20031116, "hou", "tor", "vrempsu", 48)
game <- md(20041117, "bos", "was", "vrempsu", 69)
game <- md(20050325, "mil", "gsw", "vrempsu", 76)
game <- md(20091122, "bos", "nyk", "vrempsu", 69)
game <- md(20110216, "ind", "det", "vrempsu", 95)
game <- md(20110327, "sac", "phi", "vrempsu", 77)

game <- md(20090202, "sas", "gsw", "vstptsu", 67)
game <- md(20090313, "det", "tor", "vstptsu", 84)
game <- md(20090313, "cle", "sac", "vstptsu", 96)
game <- md(20110406, "orl", "cha", "vstptsu", 94)
game <- md(20120421, "orl", "uta", "vstptsu", 97)
game <- md(20121231, "mia", "orl", "vstptsu", 84)
game <- md(20130304, "uta", "mil", "vstptsu", 45)

game <- md(20041130, "nyk", "atl", "vreptsu", 13)
game <- md(20050417, "atl", "nyk", "vreptsu", 38)
game <- md(20060429, "dal", "mem", "vreptsu", 16)
game <- md(20090313, "det", "tor", "vreptsu", 15)
game <- md(20090313, "cle", "sac", "vreptsu", 30)
game <- md(20101103, "mil", "bos", "vreptsu", 38)
game <- md(20120306, "lal", "det", "vreptsu", 7)
game <- md(20121231, "mia", "orl", "vreptsu", 28)
game <- md(20130210, "den", "bos", "vreptsu", 40)

game <- md(20070405, "mia", "cle", "vpmav", 2.2)
game <- md(20140309, "den", "nop", "vpmav", -1.8)

game <- md(20081121, "orl", "ind", "happ", 9)

game <- md(20060212, "den", "sea", "hstmpsu", 190)
game <- md(20070314, "pho", "dal", "hstmpsu", 189)
game <- md(20071228, "orl", "mia", "hstmpsu", 210)
game <- md(20080109, "ind", "pho", "hstmpsu", 189)
game <- md(20100113, "sas", "okc", "hstmpsu", 190)
game <- md(20110201, "hou", "lal", "hstmpsu", 203)
game <- md(20130512, "sas", "gsw", "hstmpsu", 193)
game <- md(20131111, "tor", "hou", "hstmpsu", 186)
game <- md(20141226, "hou", "mem", "hstmpsu", 158)
game <- md(20170131, "nop", "tor", "hstmpsu", 189)
game <- md(20030110, "chi", "mil", "hrempsu", 125)
game <- md(20040228, "gsw", "chi", "hrempsu", 34)
game <- md(20061207, "pho", "njn", "hrempsu", 119)
game <- md(20070119, "mia", "phi", "hrempsu", 87)
game <- md(20100304, "lal", "mia", "hrempsu", 80)
game <- md(20101124, "sas", "min", "hrempsu", 80)
game <- md(20130512, "sas", "gsw", "hrempsu", 72)
game <- md(20131111, "tor", "hou", "hrempsu", 104)
game <- md(20131218, "cha", "tor", "hrempsu", 98)
game <- md(20150415, "was", "cle", "hrempsu", 188)

game <- md(20090313, "det", "tor", "hstptsu", 75)
game <- md(20090313, "cle", "sac", "hstptsu", 95)
game <- md(20091104, "dal", "noh", "hstptsu", 83)
game <- md(20120110, "mia", "gsw", "hstptsu", 72)
game <- md(20131110, "was", "okc", "hstptsu", 80)
game <- md(20101124, "sas", "min", "hstptsu", 87)

game <- md(20030404, "nyk", "uta", "hreptsu", 26)
game <- md(20040131, "chi", "por", "hreptsu", 25)
game <- md(20080114, "lal", "sea", "hreptsu", 42)
game <- md(20080222, "sac", "cha", "hreptsu", 47)
game <- md(20090313, "det", "tor", "hreptsu", 20)
game <- md(20090313, "cle", "sac", "hreptsu", 28)
game <- md(20101203, "njn", "cha", "hreptsu", 14)
game <- md(20121215, "dal", "min", "hreptsu", 42)
game <- md(20131110, "was", "okc", "hreptsu", 26)
game <- md(20140110, "mia", "brk", "hreptsu", 16)

game <- md(20080127, "was", "mil", "tiesc", 341.6)
game <- md(20100524, "orl", "bos", "tiesc", 412)
game <- md(20160405, "min", "gsw", "tiesc", 93)

game <- md(20030106, "ind", "phi", "vledsc", 2756)
game <- md(20080114, "lal", "sea", "vledsc", 1658)
game <- md(20080127, "was", "mil", "vledsc", 1596.4)
game <- md(20080419, "pho", "sas", "vledsc", 2933.9)
game <- md(20091204, "tor", "was", "vledsc", 2361)
game <- md(20100131, "pho", "hou", "vledsc", 2819)
game <- md(20100524, "orl", "bos", "vledsc", 2587.8)
game <- md(20120127, "atl", "det", "vledsc", 502)

game <- md(20021115, "gsw", "lal", "hledsc", 2672)
game <- md(20040228, "gsw", "chi", "hledsc", 2533)
game <- md(20040322, "hou", "por", "hledsc", 443)
game <- md(20040406, "phi", "atl", "hledsc", 1062)
game <- md(20041213, "bos", "lac", "hledsc", 2553)
game <- md(20050209, "mia", "nyk", "hledsc", 82)
game <- md(20050213, "pho", "gsw", "hledsc", 645)
game <- md(20051209, "nok", "por", "hledsc", 893)
game <- md(20060107, "uta", "det", "hledsc", 2606)
game <- md(20060505, "cle", "was", "hledsc", 2311)
game <- md(20061207, "pho", "njn", "hledsc", 621)
game <- md(20070202, "por", "den", "hledsc", 1525)
game <- md(20070316, "por", "lal", "hledsc", 1164)
game <- md(20080419, "pho", "sas", "hledsc", 201.1)
game <- md(20090526, "cle", "orl", "hledsc", 1672)
game <- md(20100218, "den", "cle", "hledsc", 777)
game <- md(20100524, "orl", "bos", "hledsc", 180.2)
game <- md(20120422, "okc", "lal", "hledsc", 460)
game <- md(20121226, "det", "atl", "hledsc", 2846)
game <- md(20140325, "okc", "dal", "hledsc", 1813)

game <- md(20181223, "dal", "por", "ledchg4q", 4)

game <- md(20050213, "pho", "gsw", "hledsc4q", 242.7)

game <- md(20180223, "mil", "tor", "tie2m", 1)

game <- md(20071114, "orl", "cle", "tiesc2m", 39.4)

game <- md(20030404, "nyk", "uta", "lsp3", 0)

game <- md(20041225, "mia", "lal", "vgp", 28)
game <- md(20110205, "mem", "hou", "vgp", 51)

game <- md(20070208, "mil", "nok", "vgpw", 19)

game <- md(20171227, "den", "min", "hgpw", 22)

## Modify Season data using function 'md2' ==== 

md2 <- function(sn, tname, col_name, newval) {
  season[[col_name]][season$season == sn &
                    season$team == tname] <- newval
  return(season)
}

season <- md2(200405, "dal", "uspcsn", 0.75)
season <- md2(200708, "hou", "uspcsn", 0.8571)
season <- md2(200405, "pho", "uspcsn", 0.7222)
season <- md2(200405, "uta", "uspcsn", 0.5294)
season <- md2(202122, "lal", "expavsn", 8.12)
season <- md2(202122, "brk", "expavsn", 7.04)

# After executing the modification commands, regenerate the two Check-Output
# files to confirm the changes

# Still some issues remain in the updated Check-Output files and they are due
# to errors in the website. I copy them below:

# 'Check-Output_Game-Data.txt':

# Total seconds not equal to sum of tied, visitor lead and host lead seconds
# 
# Note: 'totsc' does not exist in original data
# # A tibble: 4 × 8
# date visitor host  tiesc vledsc hledsc totsc otnum
# <dbl> <chr>   <chr> <dbl>  <dbl>  <dbl> <dbl> <dbl>
# 1 20191217 brk     nop     389   1585   1229  3203     1
# 2 20190119 lal     hou      75   2886    243  3204     1
# 3 20131226 atl     cle     608   1547   1347  3502     2
# 4 20211126 sac     lal     811    873   2123  3807     3

# Outliers (by last column)
# **This is likely not an error**
# A tibble: 1 × 4
# date visitor host  hsco_q4
# <dbl> <chr>   <chr>   <dbl>
# 1 20040208 tor     gsw         2


# 'Check-Output_Season-Data.txt':

# Rows with NAs
# A tibble: 4 × 4
# season team  attsn attpgsn
# <dbl> <chr> <dbl>   <dbl>
# 1 202021 ind      NA      NA
# 2 202021 mia      NA      NA
# 3 202021 okc      NA      NA
# 4 202021 sac      NA      NA

