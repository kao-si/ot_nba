

# Load Data (Host Perspective) ####


library(tidyverse)

dat <- read_rds('Data_HP.rds')


# Analyses ####


library(fixest)
library(broom)

# Transform variable 'hwin' to a numeric variable 'nhwin'
dat <- dat %>% mutate(nhwin = case_when(hwin == 'Host Won' ~ 1,
                                        hwin == 'Host Lost' ~ 0))
# Logistic regression using glm
mod0 <- glm(hwin ~ hforced, family = binomial(), dat)
summary(mod0)

# Logistic regression using feglm
mod1 <- feglm(nhwin ~ hforced | host, family = binomial(), vcov = ~ host, dat)
summary(mod1)

tidy(mod1)
tidy(mod1, se = 'hetero')
tidy(mod1, cluster = 'host')
