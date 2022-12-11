# helper functions for Shiny app (see PH290_shinyproject.R)

library(dplyr)
library(data.table)
library(washb)



# for unadjusted binomial outcomes
runMods.unadjbin <- function(Y, tr, id, group1, group2){
  
  # unadjusted washbGLM linear probability model- risk difference
  unadj.washb.linprob <- washb_glm(Y= Y,
                                   tr= tr,
                                   pair=NULL, 
                                   id= id, 
                                   contrast=c(group1, group2), 
                                   family="gaussian", 
                                   print = FALSE)
  
  unadj.rd <- round(unadj.washb.linprob$TR,4)[c(1,2,3,6)]
  
  # unadjusted washbTMLE
  unadj.washbtmle <- washb_tmle(Y=Y,
                                tr= tr,
                                pair=NULL,
                                id= id,
                                W=NULL,
                                contrast=c(group1, group2), 
                                family="binomial",
                                print=FALSE,
                                seed=12345)
  
  unadj.tmle.rd <- round(unlist(unadj.washbtmle$estimates$ATE),4)[c(1,3,4,5)]
  
  unadj.bin.results <- rbind(unadj.rd, unadj.tmle.rd)
  colnames(unadj.bin.results) <- c("estimate", "ci_ll", "ci_ul", "pvalue")
  unadj.bin.results$models <- c("GLM", "TMLE")
  
  return(unadj.bin.results)
}

# # for adjusted binomial outcomes
# runMods.adjbin <- function(Y, tr, id, group1, group2, Wadj){
#   ## ADJUSTED ANALYSES
#   
#   # adjusted washbGLM linear probability model- risk difference 
#   adj.washb.linprob <- washb_glm(Y=Y,
#                                  tr=tr,
#                                  pair=NULL, 
#                                  W=Wadj,
#                                  id=id, 
#                                  contrast=c(group1, group2), 
#                                  family="gaussian", 
#                                  print = FALSE)
#   
#   adj.rd <- round(adj.washb.linprob$TR,4)[c(1,2,3,6)]
#   
#   
#   # adjusted washbTMLE
#   adj.washbtmle <- washb_tmle(Y=Y,
#                               tr=tr,
#                               pair=NULL,
#                               id=id, 
#                               W=Wadj,
#                               contrast=c(group1, group2), 
#                               family="binomial",
#                               Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"),
#                               print=FALSE,
#                               seed=12345)
#   
#   adj.tmle.rd <- round(unlist(adj.washbtmle$estimates$ATE),4)[c(1,3,4,5)]
#   
#   adj.bin.results <- rbind(adj.rd, adj.tmle.rd)
#   colnames(adj.bin.results) <- c("estimate", "ci_ll", "ci_ul", "pvalue")
#   adj.bin.results$models <- c("Adjusted GLM", "Adjusted TMLE")
#   
#   return(adj.bin.results)
# }

# for unadjusted continous outcomes
runMods.unadjcont <- function(Y, tr, id, group1, group2){
  
  # unadjusted washbGLM linear probability model- risk difference
  unadj.washb.linprob <- washb_glm(Y= Y,
                                   tr= tr,
                                   pair=NULL, 
                                   id= id, 
                                   contrast=c(group1, group2), 
                                   family="gaussian", 
                                   print = FALSE)
  
  unadj.rd <- round(unadj.washb.linprob$TR,4)[c(1,2,3,6)]
  
  # unadjusted washbTMLE
  unadj.washbtmle <- washb_tmle(Y=Y,
                                tr= tr,
                                pair=NULL,
                                id= id,
                                W=NULL,
                                contrast=c(group1, group2), 
                                family="gaussian",
                                print=FALSE,
                                seed=12345)
  
  unadj.tmle.rd <- round(unlist(unadj.washbtmle$estimates$ATE),4)[c(1,3,4,5)]
  
  unadj.cont.results <- rbind(unadj.rd, unadj.tmle.rd)
  colnames(unadj.cont.results) <- c("estimate", "ci_ll", "ci_ul", "pvalue")
  unadj.cont.results$models <- c("GLM", "TMLE")
  
  
  return(unadj.cont.results)
}

# # for adjusted continuous outcomes
# runMods.adjcont <- function(Y, tr, id, group1, group2, Wadj){
#   ## ADJUSTED ANALYSES
#   # adjusted washbGLM linear probability model- risk difference 
#   adj.washb.linprob <- washb_glm(Y=Y,
#                                  tr=tr,
#                                  pair=NULL, 
#                                  W=Wadj,
#                                  id=id, 
#                                  contrast=c(group1, group2), 
#                                  family="gaussian", 
#                                  print = FALSE)
#   
#   adj.rd <- round(adj.washb.linprob$TR,4)[c(1,2,3,6)]
#   
#   # adjusted washbTMLE
#   adj.washbtmle <- washb_tmle(Y=Y,
#                               tr=tr,
#                               pair=NULL,
#                               id=id, 
#                               W=Wadj,
#                               contrast=c(group1, group2), 
#                               family="gaussian",
#                               print=FALSE,
#                               seed=12345)
#   
#   adj.tmle.rd <- round(unlist(adj.washbtmle$estimates$ATE),4)[c(1,3,4,5)]
#   
#   adj.cont.results <- rbind(adj.rd, adj.tmle.rd)
#   colnames(adj.cont.results) <- c("estimate", "ci_ll", "ci_ul", "pvalue")
#   adj.cont.results$models <- c("Adjusted GLM", "Adjusted TMLE")
#   
#   return(adj.cont.results)
# }
# 





# prescreen vars
#ALL: sex, mode_of_birth, bacille_calmetteguerin_bcg_at_birth, maternal_live_births_count, maternal_tetanus_vaccinations_count_during_pregnancy, calcium, folic_acid, iron, vitamin_a, zinc, other, maternal_age_at_1st_pregnancy_years, mothers_age_years, mothers_education_level, mothers_occupation, mothers_height_cm, mothers_weight_kg, food_since_birth, floor_material, wall_material, roof_material, kitchen_location, room_count, open_drain, cooking_fuel, human_waste_facilities, shared_human_waste_facilities, drinking_water_source, drinking_water_treatment_method, after_cleaning_infants_anus, after_defecating, before_cleaning_infants_bottle, before_feeding_infant, before_feeding_self, family_income_in_local_currency, food_availabilty, persons_living_in_house_count, motherheight_impute, motherweight_impute
# prescreened: mode_of_birth, maternal_age_at_1st_pregnancy_years, mothers_education_level, before_feeding_self, food_availabilty


