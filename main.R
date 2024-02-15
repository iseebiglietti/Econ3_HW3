# Libraries
library("tidyverse")
library("knitr")
library("stargazer")
library("haven")
library("descr")
library("broom")
library("AER")

# Imports
ppen97 <- read_dta("RAW/PPEN97.dta") %>%
  rename(score_1 = scglob) %>%
  mutate(anai = 1900 + anai)

psen95 <- read_dta("RAW/PSEN95.dta") %>%
  rename(
    score_f_6 = franel,
    score_m_6 = mathel,
    score_f_9 = brevfra1,
    score_m_9 = brevmat1,
    score_fl_9 = brevlv11
  ) %>%
  mutate(
    score_f_9 = 5 * score_f_9,
    score_m_9 = 5 * score_m_9,
    score_fl_9 = 5 * score_fl_9
  )

# Function counting NAs
na_frequency <- function(x) {
  100 * mean(is.na(x))
}

summary97 <- ppen97 %>%
  select(score_1, score_f_3, score_m_3) %>%
  summarize(
    var = names(.),
    median = sapply(., median, na.rm = TRUE),
    mean = sapply(., mean, na.rm = TRUE),
    min = sapply(., min, na.rm = TRUE),
    max = sapply(., max, na.rm = TRUE),
    sd = sapply(., sd, na.rm = TRUE),
    `NAs (%)` = sapply(., na_frequency)
  )

summary95 <- psen95 %>%
  select(
    score_f_6, score_m_6, score_f_9,
    score_m_9, score_fl_9) %>%
  summarize(
    var = names(.),
    median = sapply(., median, na.rm = TRUE),
    mean = sapply(., mean, na.rm = TRUE),
    min = sapply(., min, na.rm = TRUE),
    max = sapply(., max, na.rm = TRUE),
    sd = sapply(., sd, na.rm = TRUE),
    `NAs (%)` = sapply(., na_frequency)
  )

ppen97 <- ppen97 %>%
  mutate(
    sexe = as.factor(sexe - 1),
    prior_area_1 = as.factor(prior_area_1 - 1),
    prior_area_3 = as.factor(prior_area_3 - 1),
    public_1 = as.factor(public_1 - 1),
    public_3 = as.factor(public_3 - 1),
    nati = as.factor(if_else(nati == 100, 1, 0)),
    age_1 = 12 * (1997 - anai) + 10 - mnai,
    age_3 = 12 * (yr_3 - anai) + 10 - mnai,
    relative_age = 12 - mnai
  )

psen95 <- psen95 %>%
  mutate(
    anai = 1900 + as.integer(substr(datenai, 5, 6)),
    mnai = as.integer(substr(datenai, 3, 4)),
    sexe = as.factor(as.integer(sexe) - 1),
    prior_area_6 = as.factor(prior_area_6 - 1),
    prior_area_9 = as.factor(prior_area_9 - 1),
    public_6 = as.factor(public_6 - 1),
    public_9 = as.factor(public_9 - 1),
    nati = as.factor(if_else(nateleve == 100, 1, 0)),
    age_6 = 12 * (yr_6 - anai) + 10 - mnai,
    age_9 = 12 * (yr_9 - anai) + 6 - mnai,
    relative_age = 12 - mnai
  )