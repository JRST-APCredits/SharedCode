# Preliminary analysis of ICCs for BIO and CHEM regression models in RQ3
#
# This assumes that you have just finished running the Shared Analysis R file. 
# It relies on those objects being in the environment (e.g., df_bio_full).

library(performance)
library(lme4)
# library(rstanarm)


# Biology RQ3b
## Need to include class_number_2 in df_bio_full
df_bio_full <- df_bio2 %>%
  select(st_id, numgrade_2, apscore_full, eligible_to_skip, skipped_course, aptaker,
         firstgen, lowincomeflag, female, female, urm, ethniccode_cat,
         hsgpa, mathsr, englsr, cohort, crs_term_2, class_number_2) %>%
  filter(complete.cases(.))

bio_rq3b.full_re <- lmer(numgrade_2 ~ 
                           skipped_course + factor(firstgen) + factor(lowincomeflag) + factor(female) + 
                           factor(ethniccode_cat) + scale(hsgpa) + scale(mathsr) + scale(englsr) + 
                           factor(crs_term_2) + (1 | class_number_2), 
                         data=df_bio_full)
summary(bio_rq3b.full_re)
performance::icc(bio_rq3b.full_re)

# More on ICC (see ?performance::icc() and Nakagawa et al., 2017).

# Chemistry RQ3b
## Need to include class_number_2 in df_chem_full
df_chem_full <- df_chem2 %>%
  select(st_id, numgrade_2, apscore_full, eligible_to_skip, skipped_course, aptaker,
         firstgen, lowincomeflag, female, female, urm, ethniccode_cat,
         hsgpa, mathsr, englsr, cohort, crs_term_2, class_number_2) %>%
  filter(complete.cases(.)) 

chem_rq3b.full_re <- lmer(numgrade_2 ~ 
                            skipped_course + factor(firstgen) + factor(lowincomeflag) + factor(female) + 
                            factor(ethniccode_cat) + scale(hsgpa) + scale(mathsr) + scale(englsr) + 
                            factor(crs_term_2) + (1 | class_number_2), 
                          data=df_chem_full)
summary(chem_rq3b.full_re)
performance::icc(chem_rq3b.full_re)

# Physics RQ3b -- not run
### Uncomment below to run Physics model

# df_phys_full <- df_phys2 %>%
#   select(st_id, numgrade_2, apscore_full, eligible_to_skip, skipped_course, aptaker,
#          firstgen, lowincomeflag, female, female, urm, ethniccode_cat,
#          hsgpa, mathsr, englsr, cohort, enrl_from_cohort_2, crs_term_2, class_number_2) %>%
#   filter(complete.cases(.))
# 
# phys_rq3b.full_re <- lmer(numgrade_2 ~
#                             skipped_course + factor(firstgen) + factor(lowincomeflag) + factor(female) +
#                             factor(ethniccode_cat) + scale(hsgpa) + scale(mathsr) + scale(englsr) +
#                             factor(crs_term_2) + (1 | class_number_2),
#                           data=df_phys_full)
# summary(phys_rq3b.full_re)
# performance::icc(phys_rq3b.full_re)
