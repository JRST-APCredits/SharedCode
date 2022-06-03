# Setup ####
# Load libraries
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load("tidyverse", "psych", "data.table", "broom",         # Data wrangling
              "summarytools", "corrplot",                           # Descriptive stats
               "BaylorEdPsych", "mvnmle",                           # Littles MCAR test
               "mice",                                              # Multiple imputation
               "jtools", "sjstats",                                 # Reporting Odds Ratio and Std. Betas
               "WeightIt", "cobalt", "survey",                      # Propensity score matching
               "sjPlot")                                            # Generating regression tables

## Options 
options(digits = 5)        # Show 5 decimals
options(scipen = 999)      # Turn off scientific notation
select <- dplyr::select    # Use dyplr for 'select' function

## Load clean dataset
df_clean <- read.csv("~/YOUR CLEAN DATA FILE PATH HERE")

## View data 
names(df_clean)
head(df_clean)

## Filter for student level inclusion/exclusion criteria ####
df_clean <- df_clean %>%
  # Include
  filter(transfer == 0) %>%
  filter(tookcourse_2 == 1) %>%
  filter(cohort >= 2013 & cohort <= 2018) %>%
  # Exclude
  filter(international == 0) 

## Create subset dataframes for each discipline ####
### Bio (complete info, after 2013)
df_bio2 <- df_clean %>%
  subset(discipline == "BIO") %>%
  subset(apyear >= 2013) 

### Chem (complete info, after 2014)
df_chem2 <- df_clean %>%
  subset(discipline == "CHEM") %>%
  subset(apyear >= 2014)

### Phys (complete info, after 2015)
df_phys2 <- df_clean %>%
  subset(discipline == "PHYS") %>%
  subset(apyear >= 2015) 

# Descriptive Stats ####
# Vector of Continuous Vars
contVar <- c("numgrade", "numgrade_2", "mathsr", "englsr", "hsgpa", "apscore")

## Full Dataset
view(dfSummary(df_clean))
lapply(df_clean[contVar], function(x) summary(x))

## By Discipline
### Bio
view(dfSummary(df_bio2))
lapply(df_bio2[contVar], function(x) summary(x))
### Chem
view(dfSummary(df_chem2))
lapply(df_chem2[contVar], function(x) summary(x))
### Phys
view(dfSummary(df_phys2))
lapply(df_phys2[contVar], function(x) summary(x))

# Run Models (for each discipline) #
## RQ1 - Out of everyone, who earns an AP score that makes them eligible to skip? ####
# (Formerly 1ci.)
## Correlations of demographics and eligibility
# Bio
bio_corr <- df_bio2 %>%
  mutate(white = (ethniccode_cat == 0)) %>%
  mutate(urm = (ethniccode_cat == 1)) %>%
  mutate(asian = (ethniccode_cat == 2)) %>%
  select(eligible_to_skip, firstgen, lowincomeflag, female, white, asian, urm) %>%
  filter(complete.cases(.)) %>%
  corr.test()
bio_corr
corrplot(bio_corr$r, method = "color", type = "lower", addCoef.col = "Black",
         p.mat = bio_corr$p, insig = "label_sig", sig.level = c(.001, .01, .05), pch.cex = 1)

# CHEM
chem_corr <- df_chem2 %>%
  mutate(white = (ethniccode_cat == 0)) %>%
  mutate(urm = (ethniccode_cat == 1)) %>%
  mutate(asian = (ethniccode_cat == 2)) %>%
  select(eligible_to_skip, firstgen, lowincomeflag, female, white, asian, urm) %>%
  filter(complete.cases(.)) %>%
  corr.test()
chem_corr
corrplot(chem_corr$r, method = "color", type = "lower", addCoef.col = "Black", 
         p.mat = chem_corr$p, insig = "label_sig", sig.level = c(.001, .01, .05), pch.cex = 1)

# PHYS
phys_corr <- df_phys2 %>%
  mutate(white = (ethniccode_cat == 0)) %>%
  mutate(urm = (ethniccode_cat == 1)) %>%
  mutate(asian = (ethniccode_cat == 2)) %>%
  select(eligible_to_skip, firstgen, lowincomeflag, female, white, asian, urm) %>%
  corr.test(use = "complete")
corrplot(phys_corr$r, method = "color", type = "lower", addCoef.col = "Black", 
         p.mat = phys_corr$p, insig = "label_sig", sig.level = c(.001, .01, .05), pch.cex = 1)

# Models
### BIO
bio_rq1 <- glm(eligible_to_skip ~ factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
                   scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort), 
                  data=df_bio2, binomial(link="logit"))
summ(bio_rq1, exp = T, digits = 5, confint=T) #95% CIs
summ(bio_rq1, exp = T, digits = 5, confint=F) #SEs

### CHEM
chem_rq1 <- glm(eligible_to_skip ~ factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort), 
               data=df_chem2, binomial(link="logit"))
summ(chem_rq1, exp = T, digits = 5, confint=T) #95% CIs
summ(chem_rq1, exp = T, digits = 5, confint=F) #SEs
plot_summs(chem_rq1, exp = T, digits = 5, confint=F)

### PHYS
phys_rq1 <- glm(eligible_to_skip ~ factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort), 
               data=df_phys2, binomial(link="logit"))
summ(phys_rq1, exp = T, digits = 5, confint=T)
summ(phys_rq1, exp = T, digits = 5, confint=F)

# Vizualization for RQ1
rq1_plots <- plot_summs(bio_rq1,chem_rq1, phys_rq1, scale = T, exp = T, 
                        legend.title = "RQ1: What predicts having the opportunity to skip?",
                        model.names = c("BIO", "CHEM", "PHYSICS"))
rq1_plots

## RQ2 - Out of everyone who was eligible to skip, who actually skipped? ####
# (Formerly 2a.)
### BIO
# Subset dataframe: Skip eligible
df_bio_skeligible <- df_bio2 %>%
  subset(eligible_to_skip == 1)

# View number of eligible students who skipped the course
table(df_bio_skeligible$skipped_course)

# Model
bio_rq2 <- glm(skipped_course ~ 
                  factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort), 
                  data=df_bio_skeligible, binomial(link="logit"))
summ(bio_rq2, exp = T, digits = 5, confint=T) #95% CIs
summ(bio_rq2, exp = T, digits = 5, confint=F) #SEs

### CHEM
# Subset dataframe: Skip eligible
df_chem_skeligible <- df_chem2 %>%
  subset(eligible_to_skip == 1)

# View number of eligible students who skipped the course
table(df_chem_skeligible$skipped_course)

# Model
chem_rq2 <- glm(skipped_course ~ 
                 factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort), 
               data=df_chem_skeligible, binomial(link="logit"))
summ(chem_rq2, exp = T, digits = 5, confint=T)
summ(chem_rq2, exp = T, digits = 5, confint=F)

### PHYS
# Subset dataframe: Skip eligible
df_phys_skeligible <- df_phys2 %>%
  subset(eligible_to_skip == 1)

# View number of eligible students who skipped the course
table(df_phys_skeligible$skipped_course)

# Model
phys_rq2 <- glm(skipped_course ~ 
                 # factor(firstgen) + *UNSTABLE*
                 factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort), 
               data=df_phys_skeligible, binomial(link="logit"))
summ(phys_rq2, exp = T, digits = 5, confint=T)
summ(phys_rq2, exp = T, digits = 5, confint=F)

# Vizualization for RQ2
rq2_plots <- plot_summs(bio_rq2,chem_rq2, phys_rq2, scale = T, exp = T,
                        legend.title = "RQ2: What predicts choosing to skip?",
                        model.names = c("BIO", "CHEM", "PHYSICS"))
rq2_plots

## RQ3A - Out of everyone, what was the effect of skipping on 2nd course grade (at each AP score)? 
### BIO
# Descriptive Stats
# Number of students who skipped the course at each AP score
table(df_bio2$skipped_course, df_bio2$apscore_full)
# Student course 2 grades at each AP score 
table(df_bio2$numgrade_2, df_bio2$apscore_full, df_bio2$skipped_course)
# Means and SD
df_bio2 %>% 
  group_by(apscore_full, skipped_course) %>%
  summarize(mean = mean(numgrade_2,  na.rm = T), sd = sd(numgrade_2,  na.rm = T)) 
# ANOVA
bio_rq3a <- aov(numgrade_2 ~ as.factor(apscore_full)*as.factor(skipped_course), 
                data=df_bio2)
anova(bio_rq3a)
TukeyHSD(bio_rq3a)

# Vizualizations for RQ3A
# Histogram
hist_bio <- df_bio2 %>%
  ggplot(aes(x = apscore, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = T)) +
  geom_histogram(stat='count', position = position_dodge(preserve = "single")) +
  scale_x_continuous(sec.axis = dup_axis(labels = NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels = NULL)) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill=NA),
    axis.title.x.top = element_blank(),
    axis.title.y.right = element_blank(),
    legend.position=c(0.25, 0.85),
    legend.background = element_blank(),
    legend.box.background = element_rect(color = "black")) +
  labs(x = "AP Score", y = "Number of Students", title= "Histogram of AP BIO Scores")
plot(hist_bio)

# Line Graph
plot_bio_rq3a <- df_bio2 %>%
  ggplot(aes(y = numgrade_2, x = apscore_full, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = T)) +
  geom_point(stat = 'summary', fun.y = 'mean', size=3) +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.1) +
  geom_smooth(stat = 'summary', method = 'loess') +
  scale_x_continuous(limits = c(0,5), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 5, by=1), labels=c("Didn't Take", "1", "2", "3", "4", "5")) +
  scale_y_continuous(limits = c(0,4), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 4, by=0.5)) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill=NA),
    axis.title.x.top = element_blank(),
    axis.title.y.right = element_blank(),
    legend.position=c(0.85, 0.25),
    legend.background = element_blank(),
    legend.box.background = element_rect(color = "black")
  ) +
  labs(x = "AP Score", y = "Mean Grade in BIO 2", title= "BIO Uncontrolled Model")
plot(plot_bio_rq3a)

### CHEM
# Descriptive Stats
# Number of students who skipped the course at each AP score
table(df_chem2$skipped_course, df_chem2$apscore_full)
# Student course 2 grades at each AP score 
table(df_chem2$numgrade_2, df_chem2$apscore_full, df_chem2$skipped_course) 
# Means and SD
df_chem2 %>% 
  group_by(apscore_full, skipped_course) %>%
  summarize(mean = mean(numgrade_2,  na.rm = T), sd = sd(numgrade_2,  na.rm = T)) 
# ANOVA
chem_rq3a <- aov(numgrade_2 ~ as.factor(apscore_full)*as.factor(skipped_course), 
                 data=df_chem2)
anova(chem_rq3a)
TukeyHSD(chem_rq3a)

# Vizualizations for RQ3A
# Histogram
hist_chem <- df_chem2 %>%
  ggplot(aes(x = apscore, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = T)) +
  geom_histogram(stat='count', position = position_dodge(preserve = "single")) +
  scale_x_continuous(sec.axis = dup_axis(labels = NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels = NULL)) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill=NA),
    axis.title.x.top = element_blank(),
    axis.title.y.right = element_blank(),
    legend.position=c(0.25, 0.85),
    legend.background = element_blank(),
    legend.box.background = element_rect(color = "black")) +
  labs(x = "AP Score", y = "Number of Students", title= "Histogram of AP CHEM Scores")
plot(hist_chem)

# Line Graph
plot_chem_rq3a <- df_chem2 %>%
  ggplot(aes(y = numgrade_2, x = apscore_full, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = T)) +
  geom_point(stat = 'summary', fun.y = 'mean', size=3) +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.1) +
  geom_smooth(stat = 'summary', method = 'loess') +
  scale_x_continuous(limits = c(0,5), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 5, by=1), labels=c("Didn't Take", "1", "2", "3", "4", "5")) +
  scale_y_continuous(limits = c(0,4), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 4, by=0.5)) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill=NA),
    axis.title.x.top = element_blank(),
    axis.title.y.right = element_blank(),
    legend.position=c(0.85, 0.25),
    legend.background = element_blank(),
    legend.box.background = element_rect(color = "black")
  ) +
  labs(x = "AP Score", y = "Mean Grade in CHEM 2", title= "CHEM Uncontrolled Model")
plot(plot_chem_rq3a)

### PHYS
# Descriptive Stats
# Number of students who skipped the course at each AP score
table(df_phys2$skipped_course, df_phys2$apscore_full)
# Student course 2 grades at each AP score 
table(df_phys2$numgrade_2, df_phys2$apscore_full, df_phys2$skipped_course) 
# Means and SD
df_phys2 %>% 
  group_by(apscore_full, skipped_course) %>%
  summarize(mean = mean(numgrade_2,  na.rm = T), sd = sd(numgrade_2,  na.rm = T)) 
# ANOVA
phys_rq3a <- aov(numgrade_2 ~ as.factor(apscore_full)*as.factor(skipped_course), 
                 data=df_phys2)
anova(phys_rq3a)
TukeyHSD(phys_rq3a)

# Vizualizations for RQ3A
# Histogram
hist_phys <- df_phys2 %>%
  ggplot(aes(x = apscore, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = T)) +
  geom_histogram(stat='count', position = position_dodge(preserve = "single")) +
  scale_x_continuous(sec.axis = dup_axis(labels = NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels = NULL)) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill=NA),
    axis.title.x.top = element_blank(),
    axis.title.y.right = element_blank(),
    legend.position=c(0.25, 0.85),
    legend.background = element_blank(),
    legend.box.background = element_rect(color = "black")) +
  labs(x = "AP Score", y = "Number of Students", title= "Histogram of AP PHYS Scores")
plot(hist_phys)

# Line Graph
plot_phys_rq3a <- df_phys2 %>%
  ggplot(aes(y = numgrade_2, x = apscore_full, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = T)) +
  geom_point(stat = 'summary', fun.y = 'mean', size=3) +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.1) +
  geom_smooth(stat = 'summary', method = 'loess') +
  scale_x_continuous(limits = c(0,5), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 5, by=1), labels=c("Didn't Take", "1", "2", "3", "4", "5")) +
  scale_y_continuous(limits = c(0,4), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 4, by=0.5)) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill=NA),
    axis.title.x.top = element_blank(),
    axis.title.y.right = element_blank(),
    legend.position=c(0.85, 0.25),
    legend.background = element_blank(),
    legend.box.background = element_rect(color = "black")
  ) +
  labs(x = "AP Score", y = "Mean Grade in PHYS 2", title= "PHYS Uncontrolled Model")
plot(plot_phys_rq3a)

## RQ3B - In a matched sample, what was the effect of skipping on 2nd course grade?
### BIO
# RQ3B.FULL
# Complete cases only
df_bio_full <- df_bio2 %>%
  select(st_id, numgrade_2, apscore_full, eligible_to_skip, skipped_course, aptaker,
         firstgen, lowincomeflag, female, urm, ethniccode_cat,
         hsgpa, mathsr, englsr, cohort, enrl_from_cohort_2, crs_term_2) %>%
  filter(complete.cases(.)) 
# Check balance before weighting
bal.tab(skipped_course ~ factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2),
        data = df_bio_full, estimand = "ATT", m.threshold = .05)
# Estimate weights
bio.out.full <- weightit(skipped_course ~ factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
                        scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2),
                      data = df_bio_full, estimand = "ATT")
summary(bio.out.full) 
# Check balance after weighting
bal.tab(bio.out.full, m.threshold = .05, disp.v.ratio = T)
# Extract weights
bio.w.full <- svydesign(ids = ~1, weights = bio.out.full$weights,
                     data = df_bio_full)

# Model
bio_rq3b.full <- svyglm(numgrade_2 ~ skipped_course + 
                          factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) + 
                          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                        design=bio.w.full, na.action=na.exclude)
summ(bio_rq3b.full, digits = 5, confint=T)
summ(bio_rq3b.full, digits = 5, confint=F)


# RQ3B.NSE (When score = NOT ELIGIBLE)
# Subset and select complete cases only
df_bio_nonskeligible <- df_bio2 %>%
  subset(eligible_to_skip == 0) %>%
  select(st_id, numgrade_2, apscore_full, eligible_to_skip, skipped_course, aptaker,
         firstgen, lowincomeflag, female, urm, ethniccode_cat,
         hsgpa, mathsr, englsr, cohort, enrl_from_cohort_2, crs_term_2) %>%
  filter(complete.cases(.)) 
df_bio_nonskeligible <- svydesign(id = ~1, data = df_bio_nonskeligible)

# Model
bio_rq3b.nosk <- svyglm(numgrade_2 ~ factor(apscore_full) + 
                 factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) + 
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                 design=df_bio_nonskeligible, na.action=na.exclude)
summ(bio_rq3b.nosk, digits = 5, confint=T)
summ(bio_rq3b.nosk, digits = 5, confint=F)

# RQ3B.SE (When score = ELIGIBLE)
# Subset, select complete cases only, and weight
df_bio_skeligible <- df_bio2 %>%
  subset(eligible_to_skip == 1) %>%
  select(st_id, numgrade_2, apscore_full, eligible_to_skip, skipped_course, aptaker,
         firstgen, lowincomeflag, female, urm, ethniccode_cat,
         hsgpa, mathsr, englsr, cohort, enrl_from_cohort_2, crs_term_2) %>%
  filter(complete.cases(.)) 
# Check balance before weighting
bal.tab(skipped_course ~ factor(apscore_full) + factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2),
        data = df_bio_skeligible, estimand = "ATT", m.threshold = .05)
# Estimate weights
bio.out.skeligible <- weightit(skipped_course ~ factor(apscore_full) + factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
                           scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2),
                         data = df_bio_skeligible, estimand = "ATT")
summary(bio.out.skeligible) 
# Check balance after weighting
bal.tab(bio.out.skeligible, m.threshold = .05, disp.v.ratio = T)
# Extract weights
bio.w.skeligible <- svydesign(ids = ~1, weights = bio.out.skeligible$weights,
                        data = df_bio_skeligible)

# Model
bio_rq3b.skeligible <- svyglm(numgrade_2 ~ skipped_course + 
                          factor(apscore_full) +  # Need to comment out this line if Skip Eligible Score = 5
                          factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) + 
                          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                        design=bio.w.skeligible, na.action=na.exclude)
summ(bio_rq3b.skeligible, digits = 5, confint=T)
summ(bio_rq3b.skeligible, digits = 5, confint=F)

## RQ3B.4 (When Score  = 4)
# Subset and Weight Data
df_bio_match.4 <- df_bio2 %>%
  select(st_id, numgrade_2, apscore_full, eligible_to_skip, skipped_course, aptaker,
         firstgen, lowincomeflag, female, urm, ethniccode_cat,
         hsgpa, mathsr, englsr, cohort, enrl_from_cohort_2, crs_term_2) %>%
  subset(apscore_full == 4) %>%
  filter(complete.cases(.))

# Check balance before weighting
bal.tab(skipped_course ~ factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2),
        data = df_bio_match.4, estimand = "ATT", m.threshold = .05)

# Estimate weights
bio.out.4 <- weightit(skipped_course ~ factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
                        scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2),
                      data = df_bio_match.4, estimand = "ATT")
summary(bio.out.4) 
# Check balance after weighting
bal.tab(bio.out.4, m.threshold = .05, disp.v.ratio = T)
# Extract weights
bio.w.4 <- svydesign(ids = ~1, weights = bio.out.4$weights,
                     data = df_bio_match.4)

# Model
bio_rq3b.4 <- svyglm(numgrade_2 ~ skipped_course +  
                   factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) + 
                   scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                 design=bio.w.4, na.action=na.exclude)
summ(bio_rq3b.4, digits = 5, confint=T)
summ(bio_rq3b.4, digits = 5, confint=F)

## RQ3B (When Score  = 5)
# Subset and Weight Data 
# AP Score = 5
df_bio_match.5 <- df_bio2 %>%
  select(st_id, numgrade_2, apscore_full, eligible_to_skip, skipped_course, aptaker,
         firstgen, lowincomeflag, female, urm, ethniccode_cat,
         hsgpa, mathsr, englsr, cohort, enrl_from_cohort_2, crs_term_2) %>%
  subset(apscore_full == 5) %>%
  filter(complete.cases(.))
# Check balance before weighting
bal.tab(skipped_course ~ factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2),
        data = df_bio_match.5, estimand = "ATT", m.threshold = .05)
# Estimate weights
bio.out.5 <- weightit(skipped_course ~ factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
                        scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2),
                      data = df_bio_match.5, estimand = "ATT")
summary(bio.out.5) 
# Check balance after weighting
bal.tab(bio.out.5, m.threshold = .05, disp.v.ratio = T)
# Extract weights
bio.w.5 <- svydesign(ids = ~1, weights = bio.out.5$weights,
                     data = df_bio_match.5)

# Model
bio_rq3b.5 <- svyglm(numgrade_2 ~ skipped_course +  
                       factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) + 
                       scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                     design=bio.w.5)
summ(bio_rq3b.5, digits = 5, confint=T)
summ(bio_rq3b.5, digits = 5, confint=F)

# Vizualization for RQ3B
# Estimates
rq3.bio_plots <- plot_summs(bio_rq3b.full, bio_rq3b.nosk, bio_rq3b.skeligible, bio_rq3b.4, bio_rq3b.5, scale = T,
                            legend.title = "RQ3: Effects of skipping (Bio)?",
                            model.names = c("BIO_Full", "BIO_NoSkip", "BIO_Skip", "BIO_4", "BIO_5"),
                            coefs = c("Intercept" = "(Intercept)", "Skipped Course" = "skipped_course"))
rq3.bio_plots

# Matched Line Graphs
df_rq3b_bio.nosk <- as.data.frame(bio_rq3b.nosk$model) %>%
  mutate(numgrade_2.fitted = bio_rq3b.nosk[["fitted.values"]]) %>%
  mutate(`factor(apscore_full)` = as.numeric(as.character(`factor(apscore_full)`))) %>%
  mutate(skipped_course = 0) 
df_rq3b_bio.4 <- as.data.frame(bio_rq3b.4$model) %>%
  mutate(numgrade_2.fitted = bio_rq3b.4[["fitted.values"]]) %>%
  mutate(`factor(apscore_full)` = as.numeric(4))
df_rq3b_bio.5 <- as.data.frame(bio_rq3b.5$model) %>%
  mutate(numgrade_2.fitted = bio_rq3b.5[["fitted.values"]]) %>%
  mutate(`factor(apscore_full)` = as.numeric(5))
df_rq3b_bio <-bind_rows(df_rq3b_bio.nosk, df_rq3b_bio.4, df_rq3b_bio.5)

plot_bio_rq3b <- df_rq3b_bio %>%
  ggplot(aes(y = numgrade_2.fitted, x = `factor(apscore_full)`, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = T)) +
  geom_point(stat = 'summary', fun.y = 'mean', size=3) +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.1) +
  geom_smooth(stat = 'summary', method = 'loess') +
  scale_x_continuous(limits = c(0,5), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 5, by=1), labels=c("Didn't Take", "1", "2", "3", "4", "5")) +
  scale_y_continuous(limits = c(0,4), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 4, by=0.5)) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill=NA),
    axis.title.x.top = element_blank(),
    axis.title.y.right = element_blank(),
    legend.position=c(0.85, 0.25),
    legend.background = element_blank(),
    legend.box.background = element_rect(color = "black")
  ) +
  labs(x = "AP Score", y = "Mean Grade in BIO 2", title= "BIO Matched Model")
plot(plot_bio_rq3b)

### CHEM
# RQ3B.FULL
# Complete cases only
df_chem_full <- df_chem2 %>%
  select(st_id, numgrade_2, apscore_full, eligible_to_skip, skipped_course, aptaker,
         firstgen, lowincomeflag, female, urm, ethniccode_cat,
         hsgpa, mathsr, englsr, cohort, enrl_from_cohort_2, crs_term_2) %>%
  filter(complete.cases(.)) 
# Check balance before weighting
bal.tab(skipped_course ~ factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2),
        data = df_chem_full, estimand = "ATT", m.threshold = .05)
# Estimate weights
chem.out.full <- weightit(skipped_course ~ factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
                           scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2),
                         data = df_chem_full, estimand = "ATT")
summary(chem.out.full) 
# Check balance after weighting
bal.tab(chem.out.full, m.threshold = .05, disp.v.ratio = T)
# Extract weights
chem.w.full <- svydesign(ids = ~1, weights = chem.out.full$weights,
                        data = df_chem_full)

# Model
chem_rq3b.full <- svyglm(numgrade_2 ~ skipped_course + 
                          factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) + 
                          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                        design=chem.w.full, na.action=na.exclude)
summ(chem_rq3b.full, digits = 5, confint=T)
summ(chem_rq3b.full, digits = 5, confint=F)

# RQ3B.NSE (When score = NOT ELIGIBLE)
# Subset and select complete cases only
df_chem_nonskeligible <- df_chem2 %>%
  subset(eligible_to_skip == 0) %>%
  select(st_id, numgrade_2, apscore_full, eligible_to_skip, skipped_course, aptaker,
         firstgen, lowincomeflag, female, urm, ethniccode_cat,
         hsgpa, mathsr, englsr, cohort, enrl_from_cohort_2, crs_term_2) %>%
  filter(complete.cases(.)) 
df_chem_nonskeligible <- svydesign(id = ~1, data = df_chem_nonskeligible)

# Model
chem_rq3b.nosk <- svyglm(numgrade_2 ~ factor(apscore_full) + 
                          factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) + 
                          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                        design=df_chem_nonskeligible, na.action=na.exclude)
summ(chem_rq3b.nosk, digits = 5, confint=T)
summ(chem_rq3b.nosk, digits = 5, confint=F)

# RQ3B.SE (When score = ELIGIBLE)
# Subset, select complete cases only, and weight
df_chem_skeligible <- df_chem2 %>%
  subset(eligible_to_skip == 1) %>%
  select(st_id, numgrade_2, apscore_full, eligible_to_skip, skipped_course, aptaker,
         firstgen, lowincomeflag, female, urm, ethniccode_cat,
         hsgpa, mathsr, englsr, cohort, enrl_from_cohort_2, crs_term_2) %>%
  filter(complete.cases(.)) 
# Check balance before weighting
bal.tab(skipped_course ~ factor(apscore_full) + factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2),
        data = df_chem_skeligible, estimand = "ATT", m.threshold = .05)
# Estimate weights
chem.out.skeligible <- weightit(skipped_course ~ factor(apscore_full) + factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
                                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2),
                               data = df_chem_skeligible, estimand = "ATT")
summary(chem.out.skeligible) 
# Check balance after weighting
bal.tab(chem.out.skeligible, m.threshold = .05, disp.v.ratio = T)
# Extract weights
chem.w.skeligible <- svydesign(ids = ~1, weights = chem.out.skeligible$weights,
                              data = df_chem_skeligible)

# Model
chem_rq3b.skeligible <- svyglm(numgrade_2 ~ skipped_course + 
                                factor(apscore_full) +  # Need to comment out this line if Skip Eligible Score = 5
                                factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) + 
                                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                              design=chem.w.skeligible, na.action=na.exclude)
summ(chem_rq3b.skeligible, digits = 5, confint=T)
summ(chem_rq3b.skeligible, digits = 5, confint=F)

## RQ3B.4 (When Score  = 3)
# Subset and Weight Data
df_chem_match.3 <- df_chem2 %>%
  select(st_id, numgrade_2, apscore_full, eligible_to_skip, skipped_course, aptaker,
         firstgen, lowincomeflag, female, urm, ethniccode_cat,
         hsgpa, mathsr, englsr, cohort, enrl_from_cohort_2, crs_term_2) %>%
  subset(apscore_full == 3) %>%
  filter(complete.cases(.))
# Check balance before weighting
bal.tab(skipped_course ~ factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2),
        data = df_chem_match.3, estimand = "ATT", m.threshold = .05)
# Estimate weights
chem.out.3 <- weightit(skipped_course ~ factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
                         scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2),
                       data = df_chem_match.3, estimand = "ATT")
summary(chem.out.3) 
# Check balance after weighting
bal.tab(chem.out.3, m.threshold = .05, disp.v.ratio = T)
# Extract weights
chem.w.3 <- svydesign(ids = ~st_id, weights = chem.out.3$weights,
                      data = df_chem_match.3)

# Model
chem_rq3b.3 <- svyglm(numgrade_2 ~ skipped_course +  
                        factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) + 
                        scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                      design=chem.w.3, na.action=na.exclude)
summ(chem_rq3b.3, digits = 5, confint = T)
summ(chem_rq3b.3, digits = 5, confint = F)

## RQ3B.4 (When Score  = 4)
# Subset and Weight Data
df_chem_match.4 <- df_chem2 %>%
  select(st_id, numgrade_2, apscore_full, eligible_to_skip, skipped_course, aptaker,
         firstgen, lowincomeflag, female, urm, ethniccode_cat,
         hsgpa, mathsr, englsr, cohort, enrl_from_cohort_2, crs_term_2) %>%
  subset(apscore_full == 4) %>%
  filter(complete.cases(.))
# Check balance before weighting
bal.tab(skipped_course ~ factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2),
        data = df_chem_match.4, estimand = "ATT", m.threshold = .05)
# Estimate weights
chem.out.4 <- weightit(skipped_course ~ factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
                        scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2),
                      data = df_chem_match.4, estimand = "ATT")
summary(chem.out.4) 
# Check balance after weighting
bal.tab(chem.out.4, m.threshold = .05, disp.v.ratio = T)
# Extract weights
chem.w.4 <- svydesign(ids = ~st_id, weights = chem.out.4$weights,
                     data = df_chem_match.4)

# Model
chem_rq3b.4 <- svyglm(numgrade_2 ~ skipped_course +  
                       factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) + 
                       scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                     design=chem.w.4, na.action=na.exclude)
summ(chem_rq3b.4, digits = 5, confint = T)
summ(chem_rq3b.4, digits = 5, confint = F)

## RQ3B (When Score  = 5)
# Subset and Weight Data 
# AP Score = 5
df_chem_match.5 <- df_chem2 %>%
  select(st_id, numgrade_2, apscore_full, eligible_to_skip, skipped_course, aptaker,
         firstgen, lowincomeflag, female, urm, ethniccode_cat,
         hsgpa, mathsr, englsr, cohort, enrl_from_cohort_2, crs_term_2) %>%
  subset(apscore_full == 5) %>%
  filter(complete.cases(.))
# Check balance before weighting
bal.tab(skipped_course ~ factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2),
        data = df_chem_match.5, estimand = "ATT", m.threshold = .05)
# Estimate weights
chem.out.5 <- weightit(skipped_course ~ factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
                        scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2),
                      data = df_chem_match.5, estimand = "ATT")
summary(chem.out.5) 
# Check balance after weighting
bal.tab(chem.out.5, m.threshold = .05, disp.v.ratio = T)
# Extract weights
chem.w.5 <- svydesign(ids = ~st_id, weights = chem.out.5$weights,
                     data = df_chem_match.5)

# Model
chem_rq3b.5 <- svyglm(numgrade_2 ~ skipped_course +  
                       factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) + 
                       scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                     design=chem.w.5)
summ(chem_rq3b.5, digits = 5, confint = T)
summ(chem_rq3b.5, digits = 5, confint = F)

# Vizualization for RQ3B
# Estimates
rq3.chem_plots <- plot_summs(chem_rq3b.full, chem_rq3b.nosk, chem_rq3b.skeligible, chem_rq3b.3, chem_rq3b.4, chem_rq3b.5, scale = T,
                             legend.title = "RQ3: Effects of skipping (Chem)?",
                             model.names = c("CHEM_Full", "CHEM_NoSkip", "CHEM_Skip", "CHEM_3", "CHEM_4", "CHEM_5"),
                             coefs = c("Intercept" = "(Intercept)", "Skip" = "skipped_course"))
rq3.chem_plots

# Matched Line Graphs 
df_rq3b_chem.nosk <- as.data.frame(chem_rq3b.nosk$model) %>%
  mutate(numgrade_2.fitted = chem_rq3b.nosk[["fitted.values"]]) %>%
  mutate(`factor(apscore_full)` = as.numeric(as.character(`factor(apscore_full)`))) %>%
  mutate(skipped_course = 0) 
df_rq3b_chem.3 <- as.data.frame(chem_rq3b.3$model) %>%
  mutate(numgrade_2.fitted = chem_rq3b.3[["fitted.values"]]) %>%
  mutate(`factor(apscore_full)` = as.numeric(3))
df_rq3b_chem.4 <- as.data.frame(chem_rq3b.4$model) %>%
  mutate(numgrade_2.fitted = chem_rq3b.4[["fitted.values"]]) %>%
  mutate(`factor(apscore_full)` = as.numeric(4))
df_rq3b_chem.5 <- as.data.frame(chem_rq3b.5$model) %>%
  mutate(numgrade_2.fitted = chem_rq3b.5[["fitted.values"]]) %>%
  mutate(`factor(apscore_full)` = as.numeric(5))
df_rq3b_chem <-bind_rows(df_rq3b_chem.nosk, df_rq3b_chem.3, df_rq3b_chem.4, df_rq3b_chem.5)

plot_chem_rq3b <- df_rq3b_chem %>%
  ggplot(aes(y = numgrade_2.fitted, x = `factor(apscore_full)`, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = T)) +
  geom_point(stat = 'summary', fun.y = 'mean', size=3) +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.1) +
  geom_smooth(stat = 'summary', method = 'loess') +
  scale_x_continuous(limits = c(0,5), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 5, by=1), labels=c("Didn't Take", "1", "2", "3", "4", "5")) +
  scale_y_continuous(limits = c(0,4), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 4, by=0.5)) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill=NA),
    axis.title.x.top = element_blank(),
    axis.title.y.right = element_blank(),
    legend.position=c(0.85, 0.25),
    legend.background = element_blank(),
    legend.box.background = element_rect(color = "black")
  ) +
  labs(x = "AP Score", y = "Mean Grade in CHEM 2", title= "CHEM Matched Model")
plot(plot_chem_rq3b)


### PHYS
# RQ3B.FULL
# Complete cases only
df_phys_full <- df_phys2 %>%
  select(st_id, numgrade_2, apscore_full, eligible_to_skip, skipped_course, aptaker,
         firstgen, lowincomeflag, female, urm, ethniccode_cat,
         hsgpa, mathsr, englsr, cohort, enrl_from_cohort_2, crs_term_2) %>%
  filter(complete.cases(.)) 
# Check balance before weighting
bal.tab(skipped_course ~ factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2),
        data = df_phys_full, estimand = "ATT", m.threshold = .05)
# Estimate weights
phys.out.full <- weightit(skipped_course ~ factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
                            scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2),
                          data = df_phys_full, estimand = "ATT")
summary(phys.out.full) 
# Check balance after weighting
bal.tab(phys.out.full, m.threshold = .05, disp.v.ratio = T)
# Extract weights
phys.w.full <- svydesign(ids = ~1, weights = phys.out.full$weights,
                         data = df_phys_full)

# Model
phys_rq3b.full <- svyglm(numgrade_2 ~ skipped_course + 
                           factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) + 
                           scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                         design=phys.w.full, na.action=na.exclude)
summ(phys_rq3b.full, digits = 5, confint=T)
summ(phys_rq3b.full, digits = 5, confint=F)

# RQ3B.NSE (When score = NOT ELIGIBLE)
# Subset and select complete cases only
df_phys_nonskeligible <- df_phys2 %>%
  subset(eligible_to_skip == 0) %>%
  select(st_id, numgrade_2, apscore_full, eligible_to_skip, skipped_course, aptaker,
         firstgen, lowincomeflag, female, urm, ethniccode_cat,
         hsgpa, mathsr, englsr, cohort, enrl_from_cohort_2, crs_term_2) %>%
  filter(complete.cases(.)) 
df_phys_nonskeligible <- svydesign(id = ~1, data = df_phys_nonskeligible)

# Model
phys_rq3b.nosk <- svyglm(numgrade_2 ~ factor(apscore_full) + 
                           factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) + 
                           scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                         design=df_phys_nonskeligible, na.action=na.exclude)
summ(phys_rq3b.nosk, digits = 5, confint=T)
summ(phys_rq3b.nosk, digits = 5, confint=F)

# RQ3B.SE (When score = ELIGIBLE)
# Subset, select complete cases only, and weight
df_phys_skeligible <- df_phys2 %>%
  subset(eligible_to_skip == 1) %>%
  select(st_id, numgrade_2, apscore_full, eligible_to_skip, skipped_course, aptaker,
         firstgen, lowincomeflag, female, urm, ethniccode_cat,
         hsgpa, mathsr, englsr, cohort, enrl_from_cohort_2, crs_term_2) %>%
  filter(complete.cases(.)) 
# Check balance before weighting
bal.tab(skipped_course ~ factor(apscore_full) + factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2),
        data = df_phys_skeligible, estimand = "ATT", m.threshold = .05)
# Estimate weights
phys.out.skeligible <- weightit(skipped_course ~ factor(apscore_full) + factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
                                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2),
                                data = df_phys_skeligible, estimand = "ATT")
summary(phys.out.skeligible) 
# Check balance after weighting
bal.tab(phys.out.skeligible, m.threshold = .05, disp.v.ratio = T)
# Extract weights
phys.w.skeligible <- svydesign(ids = ~1, weights = phys.out.skeligible$weights,
                               data = df_phys_skeligible)

# Model
phys_rq3b.skeligible <- svyglm(numgrade_2 ~ skipped_course + 
                                 #factor(apscore_full) +   # Need to comment out if Skip Eligible Score = 5
                                 factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethnccode_cat) + 
                                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                               design=phys.w.skeligible, na.action=na.exclude)
summ(phys_rq3b.skeligible, digits = 5, confint=T)
summ(phys_rq3b.skeligible, digits = 5, confint=F)

## RQ3B (When Score  = 5)
# Subset and Weight Data 
# AP Score = 5
df_phys_match.5 <- df_phys2 %>%
  select(st_id, numgrade_2, apscore_full, eligible_to_skip, skipped_course, aptaker,
         firstgen, lowincomeflag, female, urm, ethniccode_cat,
         hsgpa, mathsr, englsr, cohort, enrl_from_cohort_2, crs_term_2) %>%
  subset(apscore_full == 5) %>%
  filter(complete.cases(.))
# Check balance before weighting
bal.tab(skipped_course ~ factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2),
        data = df_phys_match.5, estimand = "ATT", m.threshold = .05)
# Estimate weights
phys.out.5 <- weightit(skipped_course ~ factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) +
                         scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2),
                       data = df_phys_match.5, estimand = "ATT")
summary(phys.out.5) 
# Check balance after weighting
bal.tab(phys.out.5, m.threshold = .05, disp.v.ratio = T)
# Extract weights
phys.w.5 <- svydesign(ids = ~1, weights = phys.out.5$weights,
                      data = df_phys_match.5)

# Model
phys_rq3b.5 <- svyglm(numgrade_2 ~ skipped_course +  
                        factor(firstgen) + factor(lowincomeflag) + factor(female) + factor(ethniccode_cat) + 
                        scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                      design=phys.w.5)
summ(phys_rq3b.5, digits = 5, confint=T)
summ(phys_rq3b.5, digits = 5, confint=F)

# Vizualization for RQ3B - Estimates
rq3.phys_plots <- plot_summs(phys_rq3b.full, phys_rq3b.nosk, phys_rq3b.skeligible, phys_rq3b.5, scale = T,
                             legend.title = "RQ3: Effects of skipping (Phys)?",
                             model.names = c("PHYS_Full", "PHYS_NoSkip", "PHYS_Skip", "PHYS_5"),
                             coefs = c("Intercept" = "(Intercept)", "Skipped Course" = "skipped_course"))
rq3.phys_plots

# PHYS 
df_rq3b_phys.nosk <- as.data.frame(phys_rq3b.nosk$model) %>%
  mutate(numgrade_2.fitted = phys_rq3b.nosk[["fitted.values"]]) %>%
  mutate(`factor(apscore_full)` = as.numeric(as.character(`factor(apscore_full)`))) %>%
  mutate(skipped_course = 0) 
df_rq3b_phys.5 <- as.data.frame(phys_rq3b.5$model) %>%
  mutate(numgrade_2.fitted = phys_rq3b.5[["fitted.values"]]) %>%
  mutate(`factor(apscore_full)` = as.numeric(5))
df_rq3b_phys <-bind_rows(df_rq3b_phys.nosk, df_rq3b_phys.5)

plot_phys_rq3b <- df_rq3b_phys %>%
  ggplot(aes(y = numgrade_2.fitted, x = `factor(apscore_full)`, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = T)) +
  geom_point(stat = 'summary', fun.y = 'mean', size=3) +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.1) +
  geom_smooth(stat = 'summary', method = 'loess') +
  scale_x_continuous(limits = c(0,5), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 5, by=1), labels=c("Didn't Take", "1", "2", "3", "4", "5")) +
  scale_y_continuous(limits = c(0,4), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 4, by=0.5)) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill=NA),
    axis.title.x.top = element_blank(),
    axis.title.y.right = element_blank(),
    legend.position=c(0.85, 0.25),
    legend.background = element_blank(),
    legend.box.background = element_rect(color = "black")
  ) +
  labs(x = "AP Score", y = "Mean Grade in PHYS 2", title= "PHYS Matched Model")
plot(plot_phys_rq3b)

##### EXTRAS #####

# Combined Viz of Estimates
# RQ1 and RQ2
gridExtra::grid.arrange(rq1_plots, rq2_plots, nrow = 2, ncol = 1,
                        top = "Demographics",
                        bottom = textGrob("* Odds ratios",
                                          hjust = 0,
                                          x = 1))
# RQ3
gridExtra::grid.arrange(rq3.bio_plots, rq3.chem_plots, nrow = 2, ncol = 1, 
                        top = "Skip Effects",
                        bottom = textGrob("* Std Beta",
                                          gp = gpar(fontface = 3, fontsize = 9),
                                          hjust = 0,
                                          x = 1))
