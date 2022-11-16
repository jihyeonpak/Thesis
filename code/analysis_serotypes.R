#### Analysis File pt 2 -- Serotypes ####

# Run analysis3.R first!

# outcome distribution plots for all 3 serotypes

# serotype 1 outcome: serotype1 shed at 52 wks by qRT-PCR
ggplot(dat1) +
  aes(x = assignment_group, fill = factor(shed_poliovirus_serotype_1_at_52_weeks_by_qrtpcr)) +
  geom_bar() +
  ggtitle("Distribution of Serotype 1 Shed Poliovirus Across Groups") +
  xlab("Assignment Group") +
  ylab("Count") +
  labs(fill = "Presence of shed polivirus")

# serotype 2 outcome: serotype2 shed at 52 wks by qRT-PCR
ggplot(dat1) +
  aes(x = assignment_group, fill = factor(shed_poliovirus_serotype_2_at_52_weeks_by_qrtpcr)) +
  geom_bar() +
  ggtitle("Distribution of Serotype 2 Shed Poliovirus Across Groups") +
  xlab("Assignment Group") +
  ylab("Count") +
  labs(fill = "Presence of shed polivirus")

# serotype 3 outcome: serotype3 shed at 52 wks by qRT-PCR
ggplot(dat1) +
  aes(x = assignment_group, fill = factor(shed_poliovirus_serotype_3_at_52_weeks_by_qrtpcr)) +
  geom_bar() +
  ggtitle("Distribution of Serotype 3 Shed Poliovirus Across Groups") +
  xlab("Assignment Group") +
  ylab("Count") +
  labs(fill = "Presence of shed polivirus")


# function to use for all serotypes:

# input: Y variable (each serotype) from dataframe 'data' 
# output: unadj.rr, unadj.rd, unadj.rr, unadj.tmle, adj.rd, adj.rr, adj.tmle, adj.ipcw.tmle objects with results to use for results table
runMods <- function(Y){
  # unadjusted washbGLM log binomial- risk ratio
  unadj.washb.logbin1 <- washb_glm(Y= Y,tr=data$assignment_group,
                                   pair=NULL, id=data$participantid, contrast=c("A","D"), 
                                   family=binomial(link='log'), print = FALSE)
  unadj.washb.logbin2 <- washb_glm(Y= Y,tr=data$assignment_group,
                                   pair=NULL, id=data$participantid, contrast=c("B","D"), 
                                   family=binomial(link='log'), print = FALSE)
  unadj.washb.logbin3 <- washb_glm(Y= Y,tr=data$assignment_group,
                                   pair=NULL, id=data$participantid, contrast=c("C","D"), 
                                   family=binomial(link='log'), print = FALSE)
  unadj.washb.logbin4 <- washb_glm(Y= Y,tr=data$assignment_group,
                                   pair=NULL, id=data$participantid, contrast=c("A","B"), 
                                   family=binomial(link='log'), print = FALSE)
  unadj.washb.logbin5 <- washb_glm(Y= Y,tr=data$assignment_group,
                                   pair=NULL, id=data$participantid, contrast=c("A","C"), 
                                   family=binomial(link='log'), print = FALSE)
  unadj.washb.logbin6 <- washb_glm(Y= Y,tr=data$assignment_group,
                                   pair=NULL, id=data$participantid, contrast=c("B","C"), 
                                   family=binomial(link='log'), print = FALSE)
  
  unadj.rr <- rbind(round(unadj.washb.logbin1$TR,4), round(unadj.washb.logbin2$TR,4), round(unadj.washb.logbin3$TR,4), 
                    round(unadj.washb.logbin4$TR,4),round(unadj.washb.logbin5$TR,4), round(unadj.washb.logbin6$TR,4))
  rownames(unadj.rr) <- c("A v D","B v D","C v D","A v B", "A v C", "B v C")
  
  # unadjusted washbGLM linear probability model- risk difference
  unadj.washb.linprob1 <- washb_glm(Y=Y,tr=data$assignment_group,
                                    pair=NULL, id=data$participantid, contrast=c("A","D"), 
                                    family="gaussian", print = FALSE)
  unadj.washb.linprob2 <- washb_glm(Y=Y,tr=data$assignment_group,
                                    pair=NULL, id=data$participantid, contrast=c("B","D"), 
                                    family="gaussian", print = FALSE)
  unadj.washb.linprob3 <- washb_glm(Y=Y,tr=data$assignment_group,
                                    pair=NULL, id=data$participantid, contrast=c("C","D"), 
                                    family="gaussian", print = FALSE)
  unadj.washb.linprob4 <- washb_glm(Y=Y,tr=data$assignment_group,
                                    pair=NULL, id=data$participantid, contrast=c("A","B"), 
                                    family="gaussian", print = FALSE)
  unadj.washb.linprob5 <- washb_glm(Y=Y,tr=data$assignment_group,
                                    pair=NULL, id=data$participantid, contrast=c("A","C"), 
                                    family="gaussian", print = FALSE)
  unadj.washb.linprob6 <- washb_glm(Y=Y,tr=data$assignment_group,
                                    pair=NULL, id=data$participantid, contrast=c("B","C"), 
                                    family="gaussian", print = FALSE)
  
  unadj.rd <- rbind(round(unadj.washb.linprob1$TR,4), round(unadj.washb.linprob2$TR,4), round(unadj.washb.linprob3$TR,4),
                    round(unadj.washb.linprob4$TR,4), round(unadj.washb.linprob5$TR,4), round(unadj.washb.linprob6$TR,4))
  rownames(unadj.rd) <-c("A v D","B v D","C v D","A v B", "A v C", "B v C")
  
  # unadjusted washbTMLE
  unadj.washbtmle1 <- washb_tmle(Y=Y,
                                 tr=data$assignment_group,
                                 pair=NULL,
                                 id=data$participantid, 
                                 W=NULL, 
                                 contrast=c("A","D"), 
                                 family="binomial",
                                 print=FALSE,
                                 seed=12345)
  unadj.washbtmle2 <- washb_tmle(Y=Y,
                                 tr=data$assignment_group,
                                 pair=NULL,
                                 id=data$participantid, 
                                 W=NULL, 
                                 contrast=c("B","D"), 
                                 family="binomial",
                                 print=FALSE,
                                 seed=12345)
  unadj.washbtmle3 <- washb_tmle(Y=Y,
                                 tr=data$assignment_group,
                                 pair=NULL,
                                 id=data$participantid, 
                                 W=NULL, 
                                 contrast=c("C","D"), 
                                 family="binomial",
                                 print=FALSE,
                                 seed=12345)
  unadj.washbtmle4 <- washb_tmle(Y=Y,
                                 tr=data$assignment_group,
                                 pair=NULL,
                                 id=data$participantid, 
                                 W=NULL, 
                                 contrast=c("A","B"), 
                                 family="binomial",
                                 print=FALSE,
                                 seed=12345)
  unadj.washbtmle5 <- washb_tmle(Y=Y,
                                 tr=data$assignment_group,
                                 pair=NULL,
                                 id=data$participantid, 
                                 W=NULL, 
                                 contrast=c("A","C"), 
                                 family="binomial",
                                 print=FALSE,
                                 seed=12345)
  unadj.washbtmle6 <- washb_tmle(Y=Y,
                                 tr=data$assignment_group,
                                 pair=NULL,
                                 id=data$participantid, 
                                 W=NULL, 
                                 contrast=c("B","C"), 
                                 family="binomial",
                                 print=FALSE,
                                 seed=12345)
  
  
  unadj.tmle.rr <- rbind(round(unlist(unadj.washbtmle1$estimates$RR),4),round(unlist(unadj.washbtmle2$estimates$RR),4),
                         round(unlist(unadj.washbtmle3$estimates$RR),4),round(unlist(unadj.washbtmle4$estimates$RR),4),
                         round(unlist(unadj.washbtmle5$estimates$RR),4),round(unlist(unadj.washbtmle6$estimates$RR),4))
  rownames(unadj.tmle.rr) <-c("A v D","B v D","C v D","A v B", "A v C", "B v C")
  
  unadj.tmle.rd <- rbind(round(unlist(unadj.washbtmle1$estimates$ATE),4),round(unlist(unadj.washbtmle2$estimates$ATE),4),
                         round(unlist(unadj.washbtmle3$estimates$ATE),4),round(unlist(unadj.washbtmle4$estimates$ATE),4),
                         round(unlist(unadj.washbtmle5$estimates$ATE),4),round(unlist(unadj.washbtmle6$estimates$ATE),4))
  rownames(unadj.tmle.rd) <-c("A v D","B v D","C v D","A v B", "A v C", "B v C")
  
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
  prescreened_varnames<-washb_prescreen(Y=Y,Ws = data[Wadj],family=poisson(link='log'), pval=0.2)
  prescreened_vars <- subset(data[Wadj], select = prescreened_varnames)
  
  
  # adjusted washbGLM -risk ratio 
  # log binomial model fails to converge --> used poisson 
  adj.washb.logbin1 <- washb_glm(Y=Y,
                                 tr=data$assignment_group,pair=NULL, W=data[Wadj], 
                                 id=data$participantid, contrast=c("A","D"), 
                                 family=poisson(link='log'),
                                 print = FALSE)
  adj.washb.logbin2 <- washb_glm(Y=Y,
                                 tr=data$assignment_group,pair=NULL, W=data[Wadj], 
                                 id=data$participantid, contrast=c("B","D"), 
                                 family=poisson(link='log'),
                                 print = FALSE)
  adj.washb.logbin3 <- washb_glm(Y=Y,
                                 tr=data$assignment_group,pair=NULL, W=data[Wadj], 
                                 id=data$participantid, contrast=c("C","D"), 
                                 family=poisson(link='log'),
                                 print = FALSE)
  adj.washb.logbin4 <- washb_glm(Y=Y,
                                 tr=data$assignment_group,pair=NULL, W=data[Wadj], 
                                 id=data$participantid, contrast=c("A","B"), 
                                 family=poisson(link='log'),
                                 print = FALSE)
  adj.washb.logbin5 <- washb_glm(Y=Y,
                                 tr=data$assignment_group,pair=NULL, W=data[Wadj], 
                                 id=data$participantid, contrast=c("A","C"), 
                                 family=poisson(link='log'),
                                 print = FALSE)
  adj.washb.logbin6 <- washb_glm(Y=Y,
                                 tr=data$assignment_group,pair=NULL, W=data[Wadj], 
                                 id=data$participantid, contrast=c("B","C"), 
                                 family=poisson(link='log'),
                                 print = FALSE)
  
  adj.rr <- rbind(round(adj.washb.logbin1$TR,4), round(adj.washb.logbin2$TR,4), round(adj.washb.logbin3$TR,4), 
                  round(adj.washb.logbin4$TR,4),round(adj.washb.logbin5$TR,4), round(adj.washb.logbin6$TR,4))
  rownames(adj.rr) <- c("A v D","B v D","C v D","A v B", "A v C", "B v C")
  
  # adjusted washbGLM linear probability model- risk difference 
  adj.washb.linprob1 <- washb_glm(Y=Y,tr=data$assignment_group,
                                  pair=NULL, W=data[Wadj],id=data$participantid, contrast=c("A","D"), 
                                  family="gaussian", print = FALSE)
  adj.washb.linprob2 <- washb_glm(Y=Y,tr=data$assignment_group,
                                  pair=NULL, W=data[Wadj],id=data$participantid, contrast=c("B","D"), 
                                  family="gaussian", print = FALSE)
  adj.washb.linprob3 <- washb_glm(Y=Y,tr=data$assignment_group,
                                  pair=NULL, W=data[Wadj],id=data$participantid, contrast=c("C","D"), 
                                  family="gaussian", print = FALSE)
  adj.washb.linprob4 <- washb_glm(Y=Y,tr=data$assignment_group,
                                  pair=NULL, W=data[Wadj],id=data$participantid, contrast=c("A","B"), 
                                  family="gaussian", print = FALSE)
  adj.washb.linprob5 <- washb_glm(Y=Y,tr=data$assignment_group,
                                  pair=NULL, W=data[Wadj],id=data$participantid, contrast=c("A","C"), 
                                  family="gaussian", print = FALSE)
  adj.washb.linprob6 <- washb_glm(Y=Y,tr=data$assignment_group,
                                  pair=NULL, W=data[Wadj],id=data$participantid, contrast=c("B","C"), 
                                  family="gaussian", print = FALSE)
  
  adj.rd <- rbind(round(adj.washb.linprob1$TR,4), round(adj.washb.linprob2$TR,4), round(adj.washb.linprob3$TR,4),
                  round(adj.washb.linprob4$TR,4), round(adj.washb.linprob5$TR,4), round(adj.washb.linprob6$TR,4))
  rownames(adj.rd) <-c("A v D","B v D","C v D","A v B", "A v C", "B v C")
  
  # adjusted washbTMLE
  adj.washbtmle1 <- washb_tmle(Y=Y,
                               tr=data$assignment_group,
                               pair=NULL,
                               id=data$participantid, 
                               W=data[Wadj], 
                               contrast=c("A","D"), 
                               family="binomial",
                               Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"),
                               print=FALSE,
                               seed=12345)
  adj.washbtmle2 <- washb_tmle(Y=Y,
                               tr=data$assignment_group,
                               pair=NULL,
                               id=data$participantid, 
                               W=data[Wadj], 
                               contrast=c("B","D"), 
                               family="binomial",
                               Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"),
                               print=FALSE,
                               seed=12345)
  adj.washbtmle3 <- washb_tmle(Y=Y,
                               tr=data$assignment_group,
                               pair=NULL,
                               id=data$participantid, 
                               W=data[Wadj], 
                               contrast=c("C","D"), 
                               family="binomial",
                               Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"),
                               print=FALSE,
                               seed=12345)
  adj.washbtmle4 <- washb_tmle(Y=Y,
                               tr=data$assignment_group,
                               pair=NULL,
                               id=data$participantid, 
                               W=data[Wadj], 
                               contrast=c("A","B"), 
                               family="binomial",
                               Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"),
                               print=FALSE,
                               seed=12345)
  adj.washbtmle5 <- washb_tmle(Y=Y,
                               tr=data$assignment_group,
                               pair=NULL,
                               id=data$participantid, 
                               W=data[Wadj], 
                               contrast=c("A","C"), 
                               family="binomial",
                               Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"),
                               print=FALSE,
                               seed=12345)
  adj.washbtmle6 <- washb_tmle(Y=Y,
                               tr=data$assignment_group,
                               pair=NULL,
                               id=data$participantid, 
                               W=data[Wadj], 
                               contrast=c("B","C"), 
                               family="binomial",
                               Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"),
                               print=FALSE,
                               seed=12345)
  
  
  adj.tmle.rr <- rbind(round(unlist(adj.washbtmle1$estimates$RR),4),round(unlist(adj.washbtmle2$estimates$RR),4),
                       round(unlist(adj.washbtmle3$estimates$RR),4),round(unlist(adj.washbtmle4$estimates$RR),4),
                       round(unlist(adj.washbtmle5$estimates$RR),4),round(unlist(adj.washbtmle6$estimates$RR),4))
  rownames(adj.tmle.rr) <-c("A v D","B v D","C v D","A v B", "A v C", "B v C")
  
  adj.tmle.rd <- rbind(round(unlist(adj.washbtmle1$estimates$ATE),4),round(unlist(adj.washbtmle2$estimates$ATE),4),
                       round(unlist(adj.washbtmle3$estimates$ATE),4),round(unlist(adj.washbtmle4$estimates$ATE),4),
                       round(unlist(adj.washbtmle5$estimates$ATE),4),round(unlist(adj.washbtmle6$estimates$ATE),4))
  rownames(adj.tmle.rd) <-c("A v D","B v D","C v D","A v B", "A v C", "B v C")
  
  # adjusted washbTMLE + IPCW
  # full data
  fulldata <- data
  fulldata$Delta <- ifelse(is.na(fulldata$shed_poliovirus_at_52_weeks_by_qrtpcr),0,1)
  fulldata$Ydelta <- Y
  fulldata$Ydelta[fulldata$Delta==0] <- 9
  
  # IPCW-TMLE param
  washb.ipcw1 <- washb_tmle(Delta=fulldata$Delta, tr=fulldata$assignment_group, 
                            id=fulldata$participantid, pair=NULL,
                            Y=fulldata$Ydelta, family="gaussian",
                            Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"),
                            contrast=c("A","D"), 
                            W=fulldata[Wadj],
                            print = FALSE,
                            seed=12345)
  washb.ipcw2 <- washb_tmle(Delta=fulldata$Delta, tr=fulldata$assignment_group, 
                            id=fulldata$participantid, pair=NULL,
                            Y=fulldata$Ydelta, family="gaussian",
                            Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"),
                            contrast=c("B","D"), 
                            W=fulldata[Wadj], 
                            print = FALSE,
                            seed=12345)
  washb.ipcw3 <- washb_tmle(Delta=fulldata$Delta, tr=fulldata$assignment_group, 
                            id=fulldata$participantid, pair=NULL,
                            Y=fulldata$Ydelta, family="gaussian",
                            Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"),
                            contrast=c("C","D"), 
                            W=fulldata[Wadj], 
                            print = FALSE,
                            seed=12345)
  washb.ipcw4 <- washb_tmle(Delta=fulldata$Delta, tr=fulldata$assignment_group, 
                            id=fulldata$participantid, pair=NULL,
                            Y=fulldata$Ydelta, family="gaussian",
                            Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"),
                            contrast=c("A","B"), 
                            W=fulldata[Wadj],
                            print = FALSE,
                            seed=12345)
  washb.ipcw5 <- washb_tmle(Delta=fulldata$Delta, tr=fulldata$assignment_group, 
                            id=fulldata$participantid, pair=NULL,
                            Y=fulldata$Ydelta, family="gaussian",
                            Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"),
                            contrast=c("A","C"), 
                            W=fulldata[Wadj], 
                            print = FALSE,
                            seed=12345)
  washb.ipcw6 <- washb_tmle(Delta=fulldata$Delta, tr=fulldata$assignment_group, 
                            id=fulldata$participantid, pair=NULL,
                            Y=fulldata$Ydelta, family="gaussian",
                            Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"),
                            contrast=c("B","C"), 
                            W=fulldata[Wadj], 
                            print = FALSE,
                            seed=12345)
  
  adj.ipcw.rd <- rbind(round(unlist(washb.ipcw1$estimates$ATE),4),round(unlist(washb.ipcw2$estimates$ATE),4),
                       round(unlist(washb.ipcw3$estimates$ATE),4),round(unlist(washb.ipcw4$estimates$ATE),4),
                       round(unlist(washb.ipcw5$estimates$ATE),4),round(unlist(washb.ipcw6$estimates$ATE),4))
  rownames(adj.ipcw.rd) <-c("A v D","B v D","C v D","A v B", "A v C", "B v C")
  
  return(list(unadj.rd, unadj.rr, unadj.tmle.rd, unadj.tmle.rr, adj.rd, adj.rr, adj.tmle.rd, adj.tmle.rr, adj.ipcw.rd, prescreened_varnames))
}




# for serotype 1
type1_res <- runMods(Y = data$shed_poliovirus_serotype_1_at_52_weeks_by_qrtpcr)

type1.unadj.rd <- type1_res[[1]]
type1.unadj.rr <- type1_res[[2]]
type1.unadj.tmle.rd<- type1_res[[3]]
type1.unadj.tmle.rr<- type1_res[[4]]
type1.adj.rd<- type1_res[[5]]
type1.adj.rr<- type1_res[[6]]
type1.adj.tmle.rd<- type1_res[[7]]
type1.adj.tmle.rr<- type1_res[[8]]
type1.adj.ipcw.rd<- type1_res[[9]]
type1.prescreened_varnames<- type1_res[[10]]


# for serotype 2
type2_res <- runMods(Y = data$shed_poliovirus_serotype_2_at_52_weeks_by_qrtpcr)

type2.unadj.rd <- type2_res[[1]]
type2.unadj.rr <- type2_res[[2]]
type2.unadj.tmle.rd<- type2_res[[3]]
type2.unadj.tmle.rr<- type2_res[[4]]
type2.adj.rd<- type2_res[[5]]
type2.adj.rr<- type2_res[[6]]
type2.adj.tmle.rd<- type2_res[[7]]
type2.adj.tmle.rr<- type2_res[[8]]
type2.adj.ipcw.rd<- type2_res[[9]]
type2.prescreened_varnames<- type2_res[[10]]


# for serotype 3
type3_res <- runMods(Y = data$shed_poliovirus_serotype_3_at_52_weeks_by_qrtpcr)



type3.unadj.rd <- type3_res[[1]]
type3.unadj.rr <- type3_res[[2]]
type3.unadj.tmle.rd<- type3_res[[3]]
type3.unadj.tmle.rr<- type3_res[[4]]
type3.adj.rd<- type3_res[[5]]
type3.adj.rr<- type3_res[[6]]
type3.adj.tmle.rd<- type3_res[[7]]
type3.adj.tmle.rr<- type3_res[[8]]
type3.adj.ipcw.rd<- type3_res[[9]]
type3.prescreened_varnames<- type3_res[[10]]

