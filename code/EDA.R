## EDA - cohort characteristic figures

#### table 1
library(table1)
library(tidyverse)
library(data.table)

p <- fread("../data/participants_charac.txt")
h <- fread("../data/household_charac.txt")
r <- fread("../data/repeated_charac.txt")

# filter for first day of enrollment measures (there are 2 duplicates, each duplicate record has one missing --> dropped)
r <- r %>% filter(`Time since enrollment (days) [EUPATH_0000191]` == 0,
                  !is.na(`Weight-for-age z-score [EUPATH_0000733]`)) %>% 
  select(c(-1, -6))

# joining all datasets into one
# intermediate dataset
joined <- left_join(p, h, by = c("Household_ID"="Household_ID"))

#table 1 datasets - all participants
dat <- left_join(joined, r, by = c("Participant_ID"="Participant_ID", 
                                   "Household_ID"="Household_ID")) %>% 
  rename(
    poliovaccrand = `Poliovirus vaccine randomization [EUPATH_0036429]`,
    rotavaccrand = `Rotarix vaccine randomization [EUPATH_0036422]`,
    age = `Age at enrollment (days) [EUPATH_0000120]`,
    poliopp = `Polio vaccination per protocol [EUPATH_0036427]`,
    withdrawalreason = `Reason for withdrawal [EUPATH_0000208]`,
    sex = `Sex [PATO_0000047]`,
    birthmode = `Mode of birth [EUPATH_0036567]`,
    livebirths = `Maternal live births count [EUPATH_0036086]`,
    mage = `Mother's age (years) [EUPATH_0035109]`,
    mage1preg = `Maternal age at 1st pregnancy (years) [EUPATH_0036079]`,
    meduc = `Mother's education level [EUPATH_0036220]`,
    moccup = `Mother's occupation [EUPATH_0036221]`,
    mheight = `Mother's height (cm) [EUPATH_0036148]`,
    mweight = `Mother's weight (kg) [EUPATH_0036149]`,
    foodbirth = `Food since birth [EUPATH_0036412]`,
    wastefacil = `Human waste facilities [EUPATH_0000335]`,
    watersource = `Drinking water source [ENVO_00003064]`,
    householdcount = `Persons living in house count [EUPATH_0000019]`,
    familyinc = `Family income in local currency [EUPATH_0000727]`,
    WAZ = `Weight-for-age z-score [EUPATH_0000733]`,
    HAZ = `Height-for-age z-score [EUPATH_0011894]`
  ) %>% 
  mutate_all(na_if, "") %>% 
  mutate(group = case_when(
    poliovaccrand == "OPV at 39 weeks" & rotavaccrand == "Receive Rotarix" ~ "A",
    poliovaccrand == "IPV at 39 weeks" & rotavaccrand == "Receive Rotarix" ~ "B",
    poliovaccrand == "OPV at 39 weeks" & rotavaccrand == "No Rotarix" ~ "C",
    poliovaccrand == "IPV at 39 weeks" & rotavaccrand == "No Rotarix" ~ "D"
  )) %>% 
  mutate(withdrawn = ifelse(is.na(withdrawalreason), "Not Withdrawn", "Withdrawn"))

table.data <- dat


# withdrawn participants
withdrawal.tabledat <- table.data %>% 
  filter(!is.na(withdrawalreason))


# table 1
table.data$group <- factor(table.data$group, levels = c("A", "B", "C", "D"), 
                           labels = c("No IPV replacement, received Rotarix",
                                      "IPV replacement, received Rotarix",
                                      "No IPV replacement, no Rotarix",
                                      "IPV replacement, no Rotarix"))
label(table.data$group) <- "Study arm"


label(table.data$age) <- "Age at enrollment"
units(table.data$age) <- "days"
label(table.data$sex) <- "Sex"
label(table.data$birthmode) <- "Mode of birth"
label(table.data$WAZ) <- "WAZ at enrollment"
label(table.data$HAZ) <- "HAZ at enrollment"
label(table.data$foodbirth) <- "Food since birth"

label(table.data$mage) <- "Mother's age at enrollment"
units(table.data$mage) <- "years"
label(table.data$mage1preg) <- "Mother's age at first pregnancy"
units(table.data$mage1preg) <- "years"
label(table.data$livebirths) <- "Total live births"
label(table.data$mheight) <- "Mother's height"
units(table.data$mheight) <- "cm"
label(table.data$mweight) <- "Mother's postpartum weight"
units(table.data$mweight) <- "kg"
label(table.data$meduc) <- "Mother's education"
label(table.data$moccup) <- "Mother's occupation"

label(table.data$familyinc) <- "Total monthly income"
units(table.data$familyinc) <- "Taka"
label(table.data$watersource) <- "Household water source"
label(table.data$wastefacil) <- "Household waste facility"
label(table.data$householdcount) <- "Household members"

table1(~ group + age + sex + birthmode + WAZ + HAZ + foodbirth + mage + mage1preg + livebirths +
         mheight + mweight + meduc + moccup + familyinc + watersource + wastefacil + householdcount,
       data = table.data)

# stratified by study arm groups
table1(~ age + sex + birthmode + WAZ + HAZ + foodbirth + mage + mage1preg + livebirths +
         mheight + mweight + meduc + moccup + familyinc + watersource + wastefacil + householdcount | group,
       overall = "Total", data = table.data)

# stratified by withdrawn group
table1_w <- table1(~ group + age + sex + birthmode + WAZ + HAZ + foodbirth + mage + mage1preg + livebirths +
                     mheight + mweight + meduc + moccup + familyinc + watersource + wastefacil + householdcount | withdrawn,
                   overall = "Total", data = table.data)

table1_w
# 
# library(flextable)
# library(magrittr)
# t1flex(table1_w) %>% 
#   save_as_docx(path="table1_w.docx")
