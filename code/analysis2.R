## TMLE analysis file ##

library(tidyverse)
library(data.table)
library(lubridate)

p <- fread("../data/participants_anal2.txt")
h <- fread("../data/household_anal2.txt")
# r <- fread("../data/PROVIDE_rct_Participant_repeated_measures.txt") repeated measures not included see commented portion below
# s <- fread("../data/PROVIDE_rct_Samples.txt") samples not included see commented portion below


# # quick EDA
# pw <- p %>% select(Participant_ID, `Enrollment date [EUPATH_0000151]`)
# rw <- left_join(r, pw, by = c("Participant_Id" = "Participant_ID")) %>% 
#   mutate(
#     `Enrollment date [EUPATH_0000151]` = as_date(`Enrollment date [EUPATH_0000151]`),
#     `Observation date [EUPATH_0004991]` = as_date(`Observation date [EUPATH_0004991]`),
#     WAZ_wk = round(as.double(difftime(`Observation date [EUPATH_0004991]`, `Enrollment date [EUPATH_0000151]`, units = "weeks")), 0)
#   ) 
#  
# rw_wk0 <- rw %>% 
#   filter(WAZ_wk ==0) %>% # there are week 0 measures which should be included in for covariate screenings...
#   mutate_all(na_if, "")
#   
# colSums(is.na(rw_wk0)) # but >50% missing for all repeated measures, these measures will not be included

# sw <- left_join(s, pw, by = c("Participant_Id" = "Participant_ID")) %>% 
#   mutate(
#     `Enrollment date [EUPATH_0000151]` = as_date(`Enrollment date [EUPATH_0000151]`),
#     `Blood sample collection date [EUPATH_0033147]` = as_date(`Blood sample collection date [EUPATH_0033147]`),
#     bl_wk = round(as.double(difftime(`Blood sample collection date [EUPATH_0033147]`, `Enrollment date [EUPATH_0000151]`, units = "weeks")), 0),
#     `Stool collection date [EUPATH_0036072]` = as_date(`Stool collection date [EUPATH_0036072]`),
#     stool_wk = round(as.double(difftime(`Stool collection date [EUPATH_0036072]`, `Enrollment date [EUPATH_0000151]`, units = "weeks")), 0),
#     `Urine collection date [EUPATH_0033266]` = as_date(`Urine collection date [EUPATH_0033266]`),
#     urine_wk = round(as.double(difftime(`Urine collection date [EUPATH_0033266]`, `Enrollment date [EUPATH_0000151]`, units = "weeks")), 0)
#   )
# sw_wk0 <- sw %>% 
#   filter(bl_wk ==0 | stool_wk ==0 | urine_wk ==0) # 285 obs at week 0
# colSums(is.na(sw_wk0[, c(5:335)])) # but >50% missing for all samples. lab samples will not be included


# data cleaning
dat <- left_join(p, h, by = c("Household_ID"="Household_ID")) %>% 
  rename_with(~sub(" \\[.*\\]", "", .x)) %>% 
  rename_with(~gsub("[^[:alnum:] ]", "", .x)) %>% 
  rename_with(~tolower(gsub(" ", "_", .x, fixed = TRUE))) %>% 
  mutate(assignment_group = as.factor(case_when(
    poliovirus_vaccine_randomization == "OPV at 39 weeks" & rotarix_vaccine_randomization == "Receive Rotarix" ~ "A",
    poliovirus_vaccine_randomization == "IPV at 39 weeks" & rotarix_vaccine_randomization == "Receive Rotarix" ~ "B",
    poliovirus_vaccine_randomization == "OPV at 39 weeks" & rotarix_vaccine_randomization == "No Rotarix" ~ "C",
    poliovirus_vaccine_randomization == "IPV at 39 weeks" & rotarix_vaccine_randomization == "No Rotarix" ~ "D"
  ))) %>% 
  mutate_all(na_if, "") %>% 
  # creating numerical values for all factor variables
  mutate(
    withdrawal_before_731_days_old = as.factor(ifelse(withdrawal_before_731_days_old == "Yes", 1, 0)),
    polio_vaccination_per_protocol = as.factor(ifelse(polio_vaccination_per_protocol == "Yes", 1, 0)),
    sex = as.factor(ifelse(sex == "Male", 1, 0)), 
    mode_of_birth = as.factor(ifelse(mode_of_birth == "Caesarian", 1, 0)),
    bacille_calmetteguerin_bcg_at_birth = as.factor(ifelse(bacille_calmetteguerin_bcg_at_birth == "Yes", 1, 0)),
    maternal_tetanus_vaccinations_count_during_pregnancy = as.factor(case_when(
      maternal_tetanus_vaccinations_count_during_pregnancy == "One dose" ~ 1,
      maternal_tetanus_vaccinations_count_during_pregnancy == "Two doses" ~ 2,
      maternal_tetanus_vaccinations_count_during_pregnancy == "None" ~ 3
    )),
    calcium = as.factor(ifelse(calcium == "Yes", 1, 0)),
    folic_acid = as.factor(ifelse(folic_acid == "Yes", 1, 0)),
    iron = as.factor(ifelse(iron == "Yes", 1, 0)),
    vitamin_a = as.factor(ifelse(vitamin_a == "Yes", 1, 0)),
    zinc = as.factor(ifelse(zinc == "Yes", 1, 0)),
    other = as.factor(ifelse(other == "Yes", 1, 0)),
    mothers_education_level = case_when(
      mothers_education_level == "No formal education" ~ 0,
      mothers_education_level == "Passed class 1" ~ 1,
      mothers_education_level == "Passed class 2"~ 2,
      mothers_education_level == "Passed class 3" ~ 3,
      mothers_education_level == "Passed class 4" ~ 4,
      mothers_education_level == "Passed class 5" ~ 5,
      mothers_education_level == "Passed class 6" ~ 6,
      mothers_education_level == "Passed class 7" ~ 7,
      mothers_education_level == "Passed class 8" ~ 8,
      mothers_education_level == "Passed class 9" ~ 9,
      mothers_education_level == "Passed HSC/Fazil" ~ 10,
      mothers_education_level == "Passed Hons (3 or 4 yr hons)" ~ 11,
      mothers_education_level == "Passed SSC/Dakhil" ~ 12,
      mothers_education_level == "Vocational (Diploma, Homeopathy, LMF, etc.)" ~ 13,
      mothers_education_level == "Passed degree/Alim" ~ 14,
      mothers_education_level == "Passed Master/Kamil (MA/MSc/MCom/MBA)" ~ 15
    ),
    mothers_occupation = unclass(factor(mothers_occupation)),
    food_since_birth = unclass(factor(food_since_birth)),
    shed_poliovirus_at_52_weeks = as.factor(ifelse(shed_poliovirus_at_52_weeks == "Yes", 1, 0)),
    shed_poliovirus_at_52_weeks_by_qrtpcr = as.factor(ifelse(shed_poliovirus_at_52_weeks_by_qrtpcr == "Yes", 1, 0)),
    shed_poliovirus_serotype_1_at_52_weeks_by_qrtpcr = as.factor(ifelse(shed_poliovirus_serotype_1_at_52_weeks_by_qrtpcr == "Yes", 1, 0)),
    shed_poliovirus_serotype_2_at_52_weeks_by_qrtpcr = as.factor(ifelse(shed_poliovirus_serotype_2_at_52_weeks_by_qrtpcr == "Yes", 1, 0)),
    shed_poliovirus_serotype_3_at_52_weeks_by_qrtpcr = as.factor(ifelse(shed_poliovirus_serotype_2_at_52_weeks_by_qrtpcr == "Yes", 1, 0)),
    floor_material = unclass(factor(floor_material)),
    wall_material = unclass(factor(wall_material)),
    roof_material = unclass(factor(roof_material)),
    kitchen_location = unclass(factor(kitchen_location)),
    open_drain =as.factor(ifelse(open_drain == "Yes", 1, 0)),
    cooking_fuel = unclass(factor(cooking_fuel)),
    human_waste_facilities = unclass(factor(human_waste_facilities)),
    shared_human_waste_facilities= as.factor(ifelse(shared_human_waste_facilities == "Yes", 1, 0)),
    drinking_water_source = unclass(factor(drinking_water_source)),
    drinking_water_treatment_method = unclass(factor(drinking_water_treatment_method)),
    after_cleaning_infants_anus = unclass(factor(after_cleaning_infants_anus)),
    after_defecating = unclass(factor(after_defecating)),
    before_cleaning_infants_bottle = unclass(factor(before_cleaning_infants_bottle)),
    before_feeding_infant = unclass(factor(before_feeding_infant)),
    before_feeding_self = unclass(factor(before_feeding_self)),
    food_availabilty = unclass(factor(food_availabilty))
  )



# missing covariates: 
colSums(is.na(dat))

# mothers weight and height: continuous - mean and median pretty similar -- will mean impute
summary(dat$mothers_height_cm) 
summary(dat$mothers_weight_kg)
# small_intestine_bacterial_overgrowth, gestational_age_score, small_intestine_bacterial_overgrowth_treatment missing 50%  -- will be dropped

dat1 <- dat %>% 
  # create indicator variable for values that are mean imputed
  mutate(motherheight_impute = ifelse(is.na(mothers_height_cm), 1, 0),
         motherweight_impute = ifelse(is.na(mothers_weight_kg), 1, 0)) %>% 
  # mean imputation
  mutate(mothers_height_cm = ifelse(is.na(mothers_height_cm), mean(mothers_height_cm, na.rm = TRUE), mothers_height_cm),
         mothers_weight_kg = ifelse(is.na(mothers_weight_kg), mean(mothers_weight_kg, na.rm = TRUE), mothers_weight_kg)) %>% 
  # dropping covariates with >50% missing
  select(-c(gestational_age_score, small_intestine_bacterial_overgrowth, small_intestine_bacterial_overgrowth_treatment)) 



# ---------------------------------------------------------------------------#
## ANALYSIS ##
library(ggplot2)

# primary outcome: any type shed at 52 wks by qRT-PCR
ggplot(dat1) +
  aes(x = assignment_group, fill = shed_poliovirus_at_52_weeks_by_qrtpcr) +
  geom_bar()

# primary outcome: chi-squared
chisq <- chisq.test(table(dat1$assignment_group, dat1$shed_poliovirus_at_52_weeks_by_qrtpcr))
chisq
# fail to reject null
# is there a way to do contrasts for each level of assignment group??



## UNADJUSTED ANALYSIS
library(washb)
library(tmle)
library(blm)
library(stats)
library(broom)
library(glm2)


# sort the data for perfect replication when using random splits for V-fold cross-validation (washb_tmle and adj permutation tests)
data <- dat1[order(dat1$participantid, dat1$householdid),]
# data processing
data <- data %>% 
  mutate(participantid = as.numeric(gsub("bp", "", participantid)),
         binary_assignment = ifelse(assignment_group == "D", 1, 0), # dichotomizing exposure
         tr = unclass(factor(assignment_group)) # creating numeric values for factor exposure
  )


# # unadjusted Mantel-Haenszel test -- but no stratication variable, did not run
# Y <- data$shed_poliovirus_at_52_weeks_by_qrtpcr
# A <- data$assignment_group
# h1_contrasts <- list(c("D", "A"), 
#                      c("D", "B"),
#                      c("D", "C"))
# 
# rr.h1 <- t(sapply(h1_contrasts,washb_mh,Y=Y,tr=A,measure="RR"))
# mh.rr <- washb_mh(Y=Y,tr=A, contrast=c("A","D"),strat = data$participantid, measure="RR")
# round(mh.rr,4)

# # unadjusted RD (additive risk model)
# unadj.blm <- blm(shed_poliovirus_at_52_weeks_by_qrtpcr ~ binary_assignment, data= data)
# round(confint(unadj.blm), 3) # not converging

# unadjusted binomial linear model - risk difference
unadj.glm2 <- glm2(shed_poliovirus_at_52_weeks_by_qrtpcr ~ factor(assignment_group), data = data, family = binomial(link = "identity"))
tidy(unadj.glm2, conf.int = TRUE)

# unadjusted log-binomial model - risk ratio
unadj.logbin <- glm2(shed_poliovirus_at_52_weeks_by_qrtpcr ~ factor(assignment_group), data = data, family = binomial(link = "log"))
tidy(unadj.logbin, exponentiate = TRUE, conf.int = TRUE) 

# unadjusted tmle
set.seed(777)
SL.library <- c("SL.mean","SL.glm","SL.gam","SL.glmnet")
unadj.tmle <- tmle(Y = data$shed_poliovirus_at_52_weeks_by_qrtpcr, A = data$binary_assignment, 
                   family = "binomial", Q.SL.library = SL.library) # error: have to specify Ws

# data(washb_bangladesh_enrol)
# washb_bangladesh_enrol

unadj.washbtmle <- washb_tmle(Y=data$shed_poliovirus_at_52_weeks,tr=data$binary_assignment, id = data$participantid,
                         W=NULL,contrast = c(0, 1), family = "binomial",print= TRUE, seed = 12345)
debugonce(washb_tmle) # getting a summary.factor error??








## ADJUSTED ANALYSES
# prescreening covariates
Wadj <- c("sex","mode_of_birth","bacille_calmetteguerin_bcg_at_birth","maternal_live_births_count",
          "maternal_tetanus_vaccinations_count_during_pregnancy", "calcium","folic_acid","iron","vitamin_a",
          "zinc","other","maternal_age_at_1st_pregnancy_years","mothers_age_years","mothers_education_level",
          "mothers_occupation","mothers_height_cm","mothers_weight_kg","food_since_birth","floor_material",
          "wall_material","roof_material","kitchen_location","room_count","open_drain","cooking_fuel",
          "human_waste_facilities","shared_human_waste_facilities","drinking_water_source","drinking_water_treatment_method",
          "after_cleaning_infants_anus","after_defecating","before_cleaning_infants_bottle","before_feeding_infant",
          "before_feeding_self","family_income_in_local_currency","food_availabilty",
          "persons_living_in_house_count","motherheight_impute","motherweight_impute")
setDF(data)
# washb_prescreen
# # fails to converge
# prescreened_varnames<-washb_prescreen(Y=data$shed_poliovirus_at_52_weeks_by_qrtpcr,Ws = data[Wadj],family=binomial(link='log'), pval=0.2)
prescreened_varnames<-washb_prescreen(Y=data$shed_poliovirus_at_52_weeks_by_qrtpcr,Ws = data[Wadj],family="binomial", pval=0.2)
prescreened_varnames
prescreened_vars <- subset(data[Wadj], select = prescreened_varnames)

# # poisson gets negative value error message
# prescreened_varnames2<-washb_prescreen(Y=data$shed_poliovirus_at_52_weeks_by_qrtpcr,Ws = data[Wadj],family=poisson(link="log"), pval=0.2)



