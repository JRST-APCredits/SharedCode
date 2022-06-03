# MetaRegression
#### Start Up ##### 
# Load required packages #
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load("tidyverse", "meta", "metafor", "readxl", "gridExtra")

# Import shared coefficients dataframe ####
# Download "MetaAnalysis" tab as .csv from team's Google Sheets
df_meta <- read_csv("Desktop/AP_MetaAnalysis.csv")
df_meta <- df_meta %>%
  na_if(c("-")) %>%
  na_if(c("#DIV/0!")) %>%
  mutate_at(vars("N":"EngScore_se"), as.numeric) 

# Functions ####
# Generate DF for metaplot
df_plot <- function(x) {
  as.data.frame(x) %>%
    select(studlab:pval, byvar) %>%
    mutate(w.study = x[["w.random"]]) %>%
    mutate(w.total = x[["w.random.w"]][1] + x[["w.random.w"]][2] + x[["w.random.w"]][3]) %>%
    mutate(w.random = ifelse(byvar == 'BIO', x[["w.random.w"]][1], 
                             ifelse(byvar == 'CHEM', x[["w.random.w"]][2], 
                                    ifelse(byvar == 'PHYS', x[["w.random.w"]][3], NA)))) %>%
    mutate(weights.random = w.random/w.total) %>%
    mutate(weights.study = w.study/w.total) %>%
    mutate(TE.random = ifelse(byvar == 'BIO', x[["TE.random.w"]][1], 
                              ifelse(byvar == 'CHEM', x[["TE.random.w"]][2], 
                                     ifelse(byvar == 'PHYS', x[["TE.random.w"]][3], NA)))) %>%
    mutate(seTE.random = ifelse(byvar == 'BIO', x[["seTE.random.w"]][1], 
                                ifelse(byvar == 'CHEM', x[["seTE.random.w"]][2], 
                                       ifelse(byvar == 'PHYS', x[["seTE.random.w"]][3], NA)))) %>%
    mutate(lower.random = ifelse(byvar == 'BIO', x[["lower.random.w"]][1], 
                                 ifelse(byvar == 'CHEM', x[["lower.random.w"]][2], 
                                        ifelse(byvar == 'PHYS', x[["lower.random.w"]][3], NA)))) %>%
    mutate(upper.random = ifelse(byvar == 'BIO', x[["upper.random.w"]][1], 
                                 ifelse(byvar == 'CHEM', x[["upper.random.w"]][2], 
                                        ifelse(byvar == 'PHYS', x[["upper.random.w"]][3], NA)))) %>%
    mutate(zval.random = ifelse(byvar == 'BIO', x[["zval.random.w"]][1], 
                                ifelse(byvar == 'CHEM', x[["zval.random.w"]][2], 
                                       ifelse(byvar == 'PHYS', x[["zval.random.w"]][3], NA)))) %>%
    mutate(pval.random = ifelse(byvar == 'BIO', x[["pval.random.w"]][1], 
                                ifelse(byvar == 'CHEM', x[["pval.random.w"]][2], 
                                       ifelse(byvar == 'PHYS', x[["pval.random.w"]][3], NA))))
}
# Generate metaplot
plot <- function(d) {
  d %>%
    ggplot(aes(y = reorder(byvar, desc(byvar)))) +
    geom_errorbarh(aes(xmin = `lower.random`, xmax = `upper.random`), color = "gray50", height=.2) +
    geom_point(aes(x = `TE.random`, size = `weights.random`*3), shape = "square", color= "gray50") +
    geom_point(aes(x = TE, shape = studlab, size = `weights.study`/3)) +
    theme_bw(base_size = 12) +
    theme(legend.position = "none")
}

# RQ1a ####
df_meta_rq1a <- df_meta %>%
  filter(RQ == "1a", !is.na(N))

## FG ####
meta_rq1a_fg <- metacor(FG, N,
                        data = df_meta_rq1a,
                        studlab = school,
                        comb.fixed = FALSE,
                        comb.random = TRUE,
                        sm = "ZCOR",
                        method.tau = "SJ")
meta_rq1a_fg <- update(meta_rq1a_fg, byvar = discipline, bylab = "Discipline")
meta_rq1a_fg
forest(meta_rq1a_fg, overall = T, allstudies = F,
       leftlabs = c("First Gen", "N"), 
       colgap.forest.left = "3 cm", digits.tau2 = 2)

# Meta Plot
df_fg_rq1a <- df_plot(meta_rq1a_fg)
plot_fg_rq1a <- plot(df_fg_rq1a) +
  labs(title  = "First Generation", x = "Estimate (r)", y = element_blank()) +
  geom_vline(xintercept=0, color="black", linetype="dashed", alpha=.5) +
  coord_cartesian(xlim = c(-.2, .2))
plot_fg_rq1a

## LI ####
meta_rq1a_li <- metacor(LI, N,
                        data = df_meta_rq1a,
                        studlab = school,
                        comb.fixed = FALSE,
                        comb.random = TRUE,
                        sm = "ZCOR",
                        method.tau = "SJ")
meta_rq1a_li <- update(meta_rq1a_li, byvar = discipline, bylab = "Discipline")
meta_rq1a_li

forest(meta_rq1a_li, overall = T, allstudies = F, 
       leftlabs = c("Low Income", "N"), 
       colgap.forest.left = "3 cm", digits.tau2 = 2)

# Meta Plot
df_li_rq1a <- df_plot(meta_rq1a_li)
plot_li_rq1a <- plot(df_li_rq1a) +
  labs(title  = "Low Income", x = "Estimate (r)", y = element_blank()) +
  geom_vline(xintercept=0, color="black", linetype="dashed", alpha=.5) +
  coord_cartesian(xlim = c(-.2, .2))
plot_li_rq1a

## Female ####
meta_rq1a_fem <- metacor(Female, N,
                         data = df_meta_rq1a,
                         studlab = school,
                         comb.fixed = FALSE,
                         comb.random = TRUE,
                         sm = "ZCOR",
                         method.tau = "SJ")
meta_rq1a_fem <- update(meta_rq1a_fem, byvar = discipline, bylab = "Discipline")
meta_rq1a_fem
forest(meta_rq1a_fem, overall = T, allstudies = F, 
       leftlabs = c("Female", "N"), 
       colgap.forest.left = "3 cm",digits.tau2 = 2)

# Meta Plot
df_fem_rq1a <- df_plot(meta_rq1a_fem)
plot_fem_rq1a <- plot(df_fem_rq1a) +
  labs(title  = "Female", x = "Estimate (r)", y = element_blank()) +
  geom_vline(xintercept=0, color="black", linetype="dashed", alpha=.5) +
  coord_cartesian(xlim = c(-.2, .2))
plot_fem_rq1a

## White ####
meta_rq1a_wh <- metacor(White, N,
                         data = df_meta_rq1a,
                         studlab = school,
                         comb.fixed = FALSE,
                         comb.random = TRUE,
                         sm = "ZCOR",
                         method.tau = "SJ")
meta_rq1a_wh <- update(meta_rq1a_wh, byvar = discipline, bylab = "Discipline")
meta_rq1a_wh
forest(meta_rq1a_wh, overall = T, allstudies = F, 
       leftlabs = c("White", "N"), 
       colgap.forest.left = "3 cm", digits.tau2 = 2)

# Meta Plot
df_wh_rq1a <- df_plot(meta_rq1a_wh)
plot_wh_rq1a <- plot(df_wh_rq1a) +
  labs(title  = "White", x = "Estimate (r)", y = element_blank()) +
  geom_vline(xintercept=0, color="black", linetype="dashed", alpha=.5) +
  coord_cartesian(xlim = c(-.2, .2))
plot_wh_rq1a

## URM ####
meta_rq1a_urm <- metacor(URM, N,
                         data = df_meta_rq1a,
                         studlab = school,
                         comb.fixed = FALSE,
                         comb.random = TRUE,
                         sm = "ZCOR",
                         method.tau = "SJ")
meta_rq1a_urm <- update(meta_rq1a_urm, byvar = discipline, bylab = "Discipline")
meta_rq1a_urm
forest(meta_rq1a_urm, overall = T, allstudies = F, 
       leftlabs = c("URM", "N"), 
       colgap.forest.left = "3 cm",digits.tau2 = 2)

# Meta Plot
df_urm_rq1a <- df_plot(meta_rq1a_urm)
plot_urm_rq1a <- plot(df_urm_rq1a) +
  labs(title  = "URM", x = "Estimate (r)", y = element_blank()) +
  geom_vline(xintercept=0, color="black", linetype="dashed", alpha=.5) +
  coord_cartesian(xlim = c(-.2, .2))
plot_urm_rq1a

## Asian ####
meta_rq1a_asi <- metacor(Asian, N,
                         data = df_meta_rq1a,
                         studlab = school,
                         comb.fixed = FALSE,
                         comb.random = TRUE,
                         sm = "ZCOR",
                         method.tau = "SJ")
meta_rq1a_asi <- update(meta_rq1a_asi, byvar = discipline, bylab = "Discipline")
meta_rq1a_asi
forest(meta_rq1a_asi, overall = T, allstudies = F, 
       leftlabs = c("Asian", "N"), 
       colgap.forest.left = "3 cm", digits.tau2 = 2)

# Meta Plot
df_asi_rq1a <- df_plot(meta_rq1a_asi)
plot_asi_rq1a <- plot(df_asi_rq1a) +
  labs(title  = "Asian", x = "Estimate (r)", y = element_blank()) +
  geom_vline(xintercept=0, color="black", linetype="dashed", alpha=.5) +
  coord_cartesian(xlim = c(-.2, .2))
plot_asi_rq1a

# RQ1b ####
df_meta_rq1b <- df_meta %>%
  filter(RQ == "1b", !is.na(FG_se)) %>%
  mutate_at(vars("FG":"EngScore_se"), log)
            
## FG ####
meta_rq1b_fg <- metagen(FG, FG_se,
                     data = df_meta_rq1b,
                     studlab = school,
                     comb.fixed = FALSE,
                     comb.random = TRUE,
                     sm = "OR",
                     method.tau = "SJ")
meta_rq1b_fg <- update(meta_rq1b_fg, byvar = discipline, bylab = "Discipline")
meta_rq1b_fg
forest(meta_rq1b_fg, overall = T, allstudies = F, 
       leftlabs = c("First Gen", "Est" ,"SE"), 
       colgap.forest.left = "3 cm",digits.se = 2, digits.tau2 = 2)

# Meta Plot
df_fg_rq1b <- df_plot(meta_rq1b_fg) %>%
  mutate_at(vars("TE", "lower", "upper", "TE.random", "lower.random", "upper.random"), exp)
plot_fg_rq1b <- plot(df_fg_rq1b) +
  labs(title  = "First Generation", x = "Estimate (OR)", y = element_blank()) +
  geom_vline(xintercept=1, color="black", linetype="dashed", alpha=.5) +
  coord_cartesian(xlim = c(.1, 10)) +
  scale_x_log10(breaks = c(.1, .32, 1, 3.2, 10))
plot_fg_rq1b

## LI ####
meta_rq1b_li <- metagen(LI, LI_se,
                        data = df_meta_rq1b,
                        studlab = school,
                        comb.fixed = FALSE,
                        comb.random = TRUE,
                        sm = "OR",
                        method.tau = "SJ")
meta_rq1b_li <- update(meta_rq1b_li, byvar = discipline, bylab = "Discipline")
meta_rq1b_li
forest(meta_rq1b_li, overall = T, allstudies = F, 
       leftlabs = c("Low Income", "Est" ,"SE"), 
       colgap.forest.left = "3 cm", digits.se = 2, digits.tau2 = 2)

# Meta Plot
df_li_rq1b <- df_plot(meta_rq1b_li) %>%
  mutate_at(vars("TE", "lower", "upper", "TE.random", "lower.random", "upper.random"), exp)
plot_li_rq1b <- plot(df_li_rq1b) +
  labs(title  = "Low Income", x = "Estimate (OR)", y = element_blank()) +
  geom_vline(xintercept=1, color="black", linetype="dashed", alpha=.5) +
  coord_cartesian(xlim = c(.1, 10)) +
  scale_x_log10(breaks = c(.1, .32, 1, 3.2, 10))
plot_li_rq1b

## Female ####
meta_rq1b_fem <- metagen(Female, Female_se,
                        data = df_meta_rq1b,
                        studlab = school,
                        comb.fixed = FALSE,
                        comb.random = TRUE,
                        sm = "OR",
                        method.tau = "SJ")
meta_rq1b_fem <- update(meta_rq1b_fem, byvar = discipline, bylab = "Discipline")
meta_rq1b_fem
forest(meta_rq1b_fem, overall = T, allstudies = F, 
       leftlabs = c("Female", "Est" ,"SE"), 
       colgap.forest.left = "3 cm", digits.se = 2, digits.tau2 = 2)

# Meta Plot
df_fem_rq1b <- df_plot(meta_rq1b_fem) %>%
  mutate_at(vars("TE", "lower", "upper", "TE.random", "lower.random", "upper.random"), exp)
plot_fem_rq1b <- plot(df_fem_rq1b) +
  labs(title  = "Female", x = "Estimate (OR)", y = element_blank()) +
  geom_vline(xintercept=1, color="black", linetype="dashed", alpha=.5) +
  coord_cartesian(xlim = c(.1, 10)) +
  scale_x_log10(breaks = c(.1, .32, 1, 3.2, 10))
plot_fem_rq1b

## URM ####
meta_rq1b_urm <- metagen(URM, URM_se,
                        data = df_meta_rq1b,
                        studlab = school,
                        comb.fixed = FALSE,
                        comb.random = TRUE,
                        sm = "OR",
                        method.tau = "SJ")
meta_rq1b_urm <- update(meta_rq1b_urm, byvar = discipline, bylab = "Discipline")
meta_rq1b_urm
forest(meta_rq1b_urm, overall = T, allstudies = F, 
       leftlabs = c("URM", "Est" ,"SE"), 
       colgap.forest.left = "3 cm", digits.se = 2, digits.tau2 = 2)

# Meta Plot
df_urm_rq1b <- df_plot(meta_rq1b_urm) %>%
  mutate_at(vars("TE", "lower", "upper", "TE.random", "lower.random", "upper.random"), exp)
plot_urm_rq1b <- plot(df_urm_rq1b) +
  labs(title  = "URM", x = "Estimate (OR)", y = element_blank()) +
  geom_vline(xintercept=1, color="black", linetype="dashed", alpha=.5) +
  coord_cartesian(xlim = c(.1, 10)) +
  scale_x_log10(breaks = c(.1, .32, 1, 3.2, 10))
plot_urm_rq1b

## Asian ####
meta_rq1b_asi <- metagen(Asian, Asian_se,
                         data = df_meta_rq1b,
                         studlab = school,
                         comb.fixed = FALSE,
                         comb.random = TRUE,
                         sm = "OR",
                         method.tau = "SJ")
meta_rq1b_asi <- update(meta_rq1b_asi, byvar = discipline, bylab = "Discipline")
meta_rq1b_asi
forest(meta_rq1b_asi, overall = T, allstudies = F, 
       leftlabs = c("Asian", "Est" ,"SE"), 
       colgap.forest.left = "3 cm", digits.se = 2, digits.tau2 = 2)

# Meta Plot
df_asi_rq1b <- df_plot(meta_rq1b_asi) %>%
  mutate_at(vars("TE", "lower", "upper", "TE.random", "lower.random", "upper.random"), exp)
plot_asi_rq1b <- plot(df_asi_rq1b) +
  labs(title  = "Asian", x = "Estimate (OR)", y = element_blank()) +
  geom_vline(xintercept=1, color="black", linetype="dashed", alpha=.5) +
  coord_cartesian(xlim = c(.1, 10)) +
  scale_x_log10(breaks = c(.1, .32, 1, 3.2, 10))
plot_asi_rq1b

## GPA ####
meta_rq1b_gpa <- metagen(GPA, GPA_se,
                         data = df_meta_rq1b,
                         studlab = school,
                         comb.fixed = FALSE,
                         comb.random = TRUE,
                         sm = "OR",
                         method.tau = "SJ")
meta_rq1b_gpa <- update(meta_rq1b_gpa, byvar = discipline, bylab = "Discipline")
meta_rq1b_gpa
forest(meta_rq1b_gpa, overall = T, allstudies = F, 
       leftlabs = c("GPA", "Est" ,"SE"), 
       colgap.forest.left = "3 cm", digits.se = 2, digits.tau2 = 2)

# Meta Plot
df_gpa_rq1b <- df_plot(meta_rq1b_gpa) %>%
  mutate_at(vars("TE", "lower", "upper", "TE.random", "lower.random", "upper.random"), exp)
plot_gpa_rq1b <- plot(df_gpa_rq1b) +
  labs(title  = "HS GPA", x = "Estimate (OR)", y = element_blank()) +
  geom_vline(xintercept=1, color="black", linetype="dashed", alpha=.5) +
  coord_cartesian(xlim = c(.1, 10)) +
  scale_x_log10(breaks = c(.1, .32, 1, 3.2, 10))
plot_gpa_rq1b

## MathScore ####
meta_rq1b_math <- metagen(MathScore, MathScore_se,
                         data = df_meta_rq1b,
                         studlab = school,
                         comb.fixed = FALSE,
                         comb.random = TRUE,
                         sm = "OR",
                         method.tau = "SJ")
meta_rq1b_math <- update(meta_rq1b_math, byvar = discipline, bylab = "Discipline")
meta_rq1b_math
forest(meta_rq1b_math, overall = T, allstudies = F, 
       leftlabs = c("MathScore", "Est" ,"SE"), 
       colgap.forest.left = "3 cm", digits.se = 2, digits.tau2 = 2)

# Meta Plot
df_math_rq1b <- df_plot(meta_rq1b_math) %>%
  mutate_at(vars("TE", "lower", "upper", "TE.random", "lower.random", "upper.random"), exp)
plot_math_rq1b <- plot(df_math_rq1b) +
  labs(title  = "Math Score", x = "Estimate (OR)", y = element_blank()) +
  geom_vline(xintercept=1, color="black", linetype="dashed", alpha=.5) +
  coord_cartesian(xlim = c(.1, 10)) +
  scale_x_log10(breaks = c(.1, .32, 1, 3.2, 10))
plot_math_rq1b

## EngScore ####
meta_rq1b_eng <- metagen(EngScore, EngScore_se,
                         data = df_meta_rq1b,
                         studlab = school,
                         comb.fixed = FALSE,
                         comb.random = TRUE,
                         sm = "OR",
                         method.tau = "SJ")
meta_rq1b_eng <- update(meta_rq1b_eng, byvar = discipline, bylab = "Discipline")
meta_rq1b_eng
forest(meta_rq1b_eng, overall = T, allstudies = F, 
       leftlabs = c("EngScore", "Est" ,"SE"), 
       colgap.forest.left = "3 cm", digits.se = 2, digits.tau2 = 2)

# Meta Plot
df_eng_rq1b <- df_plot(meta_rq1b_eng) %>%
  mutate_at(vars("TE", "lower", "upper", "TE.random", "lower.random", "upper.random"), exp)
plot_eng_rq1b <- plot(df_eng_rq1b) +
  labs(title  = "English Score", x = "Estimate (OR)", y = element_blank()) +
  geom_vline(xintercept=1, color="black", linetype="dashed", alpha=.5) +
  coord_cartesian(xlim = c(.1, 10)) +
  scale_x_log10(breaks = c(.1, .32, 1, 3.2, 10))
plot_eng_rq1b

# RQ2a ####
df_meta_rq2a <- df_meta %>%
  filter(RQ == "2a")

plot_rq2a <- ggplot(df_meta_rq2a, aes(x=Eligible, y=Skipped)) +
  theme_bw(base_size = 12) +
  geom_point(aes(shape=school, color=discipline), size = 5) +
  labs(x ="% Eligible to Skip", y = "% Skipped if Eligible",
       shape = "School", color = "Discipline") +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  scale_x_continuous(breaks = c(0, .2, .4, .6, .8, 1)) +
  scale_y_continuous(breaks = c(0, .2, .4, .6, .8, 1))
plot_rq2a

# RQ3(FULL) ####
df_meta_rq3.full <- df_meta %>%
  filter(RQ == "3bFull", !is.na(Skipped_se)) 

## Skip Effect ####
meta_rq3.full <- metagen(Skipped, Skipped_se,
                        data = df_meta_rq3.full,
                        studlab = school,
                        comb.fixed = FALSE,
                        comb.random = TRUE,
                        sm = "SMD",
                        method.tau = "SJ")
meta_rq3.full <- update(meta_rq3.full, byvar = discipline, bylab = "Discipline")
meta_rq3.full
forest(meta_rq3.full, overall = T, allstudies = F, 
       leftlabs = c("Skip Effect (Full)", "Est" ,"SE"), 
       colgap.forest.left = "3 cm", digits.se = 2, digits.tau2 = 2)

# Meta Plot
df_full_rq3b <- df_plot(meta_rq3.full) 
plot_full_rq3b <- plot(df_full_rq3b) +
  labs(title  = "Full Sample", x = "Estimate (Std. Beta)", y = element_blank()) +
  geom_vline(xintercept=0, color="black", linetype="dashed", alpha=.5) +
  coord_cartesian(xlim = c(-.8, .8)) 
plot_full_rq3b

# RQ3(Eligible) ####
df_meta_rq3.elig <- df_meta %>%
  filter(RQ == "3bElig", !is.na(Skipped_se)) 

## Skip Effect ####
meta_rq3.elig <- metagen(Skipped, Skipped_se,
                         data = df_meta_rq3.elig,
                         studlab = school,
                         comb.fixed = FALSE,
                         comb.random = TRUE,
                         sm = "SMD",
                         method.tau = "SJ")
meta_rq3.elig <- update(meta_rq3.elig, byvar = discipline, bylab = "Discipline")
meta_rq3.elig
forest(meta_rq3.elig, overall = T, allstudies = F, 
       leftlabs = c("Skip Effect (Eligible)", "Est" ,"SE"), 
       colgap.forest.left = "3 cm", digits.se = 2, digits.tau2 = 2)

# Meta Plot
df_elig_rq3b <- df_plot(meta_rq3.elig) 
plot_elig_rq3b <- plot(df_elig_rq3b) +
  labs(title  = "Eligible to Skip", x = "Estimate (Std. Beta)", y = element_blank()) +
  geom_vline(xintercept=0, color="black", linetype="dashed", alpha=.5) +
  coord_cartesian(xlim = c(-.8, .8)) 
plot_elig_rq3b
