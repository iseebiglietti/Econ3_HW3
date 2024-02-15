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
# Ca c'est pour la partie 2
```{r, include = F, message = F}
reg1 <- lm(score_1~age_1, ppen97)
regm3 <- lm(score_m_3~age_3, ppen97)
regf3 <- lm(score_f_3~age_3, ppen97)
```

```{r, echo = F, results = 'asis'}
stargazer(reg1, regm3, regf3, header=FALSE, type='latex', title = "Naïve OLS", dep.var.caption = "Scores", dep.var.labels = c("Year 1: Global score", "Year 3: Math", "Year 3: French"), covariate.labels = c("Age in Months (Year 1)", "Age in Months (Year 3)"))
```

#Ca c'est pour la partie 3
#First stage 
```{r, include = F, message = F}
first_stage_1 <- lm(age_1~relative_age, ppen97)
first_stage_m_3 <- lm(age_3~relative_age, ppen97)
first_stage_f_3 <- lm(age_3~relative_age, ppen97)
```
# oui c'est normal que year 3 first stages math and french soient les mêmes. c con mais on est la pour reproduire alors on reproduit.
#Reduce form

```{r, echo = F, results = 'asis'}
stargazer(first_stage_1, first_stage_m_3, first_stage_f_3, header=FALSE, type='latex', title = "First stage", dep.var.caption = "Scores", dep.var.labels = c("Year 1: Global score", "Year 3: Math", "Year 3: French"), covariate.labels = c("Age in Months (Year 1)", "Age in Months (Year 3)"))
```

#Reduced form
```{r, include = F, message = F}
reduced_form_1 <- lm(score_1~relative_age, ppen97)
reduced_form_m_3 <- lm(score_m_3~relative_age, ppen97)
reduced_form_f_3 <- lm(score_f_3~relative_age, ppen97)
```

```{r, echo = F, results = 'asis'}
stargazer(reduced_form_1, reduced_form_m_3, reduced_form_f_3, header=FALSE, type='latex', title = "First stage", dep.var.caption = "Scores", dep.var.labels = c("Year 1: Global score", "Year 3: Math", "Year 3: French"), covariate.labels = c("Age in Months (Year 1)", "Age in Months (Year 3)"))
```

# Au cas ou ca marche pas reduced form on met ca pour reduced form
```{r, include = F, message = F}
iv_reg1 <- ivreg(score_1 ~ age_1 | relative_age, ppen97)
iv_regm3 <- ivreg(score_m_3 ~ age_3 | relative_age, ppen97)
iv_regf3 <- ivreg(score_f_3 ~ age_3 | relative_age, ppen97)
```

```{r, echo = F, results = 'asis'}
stargazer(iv_reg1, iv_regm3, iv_regf3, header=FALSE, type='latex', title = "IV estimation", dep.var.caption = "Scores", dep.var.labels = c("Year 1: Global score", "Year 3: Math", "Year 3: French"), covariate.labels = c("Age in Months (Year 1)", "Age in Months (Year 3)"))
```