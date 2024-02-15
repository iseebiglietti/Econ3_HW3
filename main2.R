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



### Partie 3 ###

### 1)
#\textbf{STUVA}: Only my own assignement of assigned relative age matter for my own enrollment (if my friend has no assigned relative age, I am still going to enroll in school and pass this test, and passing the test isn't even up to me as a pupil, it's up to the school), and for my own outcome (knowing that I am assigned a relative age but my friends are not doesn't change my studdy behavior a priori).
#\textbf{Random assignement}: Not as random as we want it to be as seen before. In fact, we think again about this teacher parents' matter that deprive our instrument of independence with the outcome, here test scores, as teacher parents' children are mostly born in May and are more successful at school (correlation wildly documented).
#\textbf{Exclusion restriction}: The instrument must be uncorrelated with the unobservable determinants of test scores, or in other words, out of the data generating process. Again here, the teacher example show us that seasonality in birth month implies different socio-economic background for specific students, as well as grade retention can totally be a variable through which our instrument affects the output measured here.
#\textbf{Relevance condition}: Our instrument must be correlated to the original variable it proxies, being here the absolute age at which the pupil took the test. The correlation is very close to one when students are young, and should gradually decrease as retention gets more common. Though we can ask ourselves about the fact that skipping classes is more frequent when young, especially in high socio-economic backgroung geographic zones (as primary school system in France is geographically based and more respected than for secondary or high schools).
#\textbf{Monotonicity}: It is quite belivable that there are no defiers in this configuration, as being assigned 11 (january) will not tend to give an incentive to NOT change cohort while having a 9 would give you the incentive to skip a year. The more plausible assumption is that if you want to skip a year having been assigned 9, you will want to skip having been assigned any other number. We however expect the presence of compliers (as seen in the paper).
#The instrument is not perfect, and maybe other methods could be envisaged as the Exculsion Restriction is hardly reliable here though is very important for IVs.

### 2) 
#The benchmark model writes :
#$$s_{ig}= \alpha_g + \beta_g a_{ig} + \epsilon_{ig}$$
#where, for grade level $g$ by students $i$: 
#- $s_{ig}$ identifies test score  obtained 
#- $a_{ig}$ is  the absolute age, in month, at which the test is taken (the endogenous variable)
#- $\beta_g$ is the partial impact of $a_{ig}$ on academic performance $s_{ig}$
#- $\epsilon_{ig}$ is the error term 

#Column $(3)$ is generated based on the first stage, that writes as follow:
#$$a_{ig}= \gamma_g + \delta_g z_i +\eta_g$$
#where:
#- $z_i=12-m_i$ is the assigned relative age (our instrument)
#- $\eta_g$ the new error term (orthogonal to $z_i)
#$\delta_g$ estimates the relevance of our instruments.

#Column (4) is generated based on the reduced form, writting:
#$$s_{ig}= \lambda_g + \mu_g z_i +\nu_ig $$
#Here, $\mu_g$ measures the impact of assigned relative age (instrument) on test scores (outcome), net of grade retention, early and late entry.

#The IV estimate writes:
#$$\hat{\beta^{IV}_g} = \frac{\mu_g}{\delta_g}$$

#Our IV, as always, identifies the effect of absolute age on test scores for students that started school in their right cohort (entering CP having 6 years old before the $1$st of January). 
#To put it differently, it identifies the average treatment effect on compliers, and thus correspond to a LATE.

### 3)

#```{r, include = F, message = F}
reg1 <- lm(score_1~age_1, ppen97)
regm3 <- lm(score_m_3~age_3, ppen97)
regf3 <- lm(score_f_3~age_3, ppen97)
#```

#```{r, echo = F, results = 'asis'}
stargazer(reg1, regm3, regf3, header=FALSE, type='latex', title = "Naïve OLS", dep.var.caption = "Scores", dep.var.labels = c("Year 1: Global score", "Year 3: Math", "Year 3: French"), covariate.labels = c("Age in Months (Year 1 Exam)", "Age in Months (Year 3 Exam)"))
#```