#---------------------------------------------------------------------------------------------------------------------
#---- Info -----
# Goal: clean and prep of the data from polarization study US
# 
#---------------------------------------------------------------------------------------------------------------------
#---- Load packages ----
#library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)
library(stringr)

#---- Load data ----
data <- read_sav("3.+GRIEG+Polarization+(S1+US)_June+7,+2024_15.46.sav") 
#summary(data) 
nrow(data) 
glimpse(data) #is like a transposed version of print()

#---- Get codebook ----
# just run once 
library(sjlabelled) #dealing with labelled data
library(tibble)
library(psych)

# see labels
#get_label(data)
#options(max.print=1000000) # if some lines are ommited 
#get_labels(data$W3)

# get codebook
data.cdbk <- enframe(get_label(data))

# use more informative column names
colnames(data.cdbk) <- c("variable", "question")

# get descriptive statistics
data.desc <- data %>% describe() %>% as_tibble() %>% dplyr::select("n","min","max","mean","sd") %>%
  mutate(mean = round(mean, 2),
         sd = round(sd, 2))
# add stats to codebook 
data.cdbk <- cbind(data.cdbk,  data.desc)
data.cdbk <- data.cdbk %>% 
  mutate(question = stringr::str_replace_all(question, "[\r\n]" , " ")) # remove "enter" with a space

# save codebook (add # if it will be done)
#write.csv2(data.cdbk, "polarization study US codebook.csv", 
#         fileEncoding = "UTF-16LE")

# View data
#sjPlot::view_df(data, encoding = "UTF-8") # THIS IS JUST GREAT TO SEE THE WHOLE DATASET WITH VAR LABELS

#---- Clean data ----
# add IDs
data <- data %>%
  mutate(subj.id = row_number()) %>% #add ID
  select(subj.id, everything()) #reorder the columns - subj.id is the first column

# summarize correct responses in attention checks
att <- data %>%
  dplyr::select(., starts_with("att")) %>% names()
#data <- data %>% # TO DO: DOESN'T WORK -- EDIT LATER
#  mutate(sum.att = rowSums(., att))

#------ recode factors ----
# remove labels
data <- data %>%
  haven::zap_labels() # remove labels (otherwise cannot scale; important to use haven package)

# recode factors
# TO BE DONE 
data <- data %>%
  mutate( 
    # educ # should be recoded, now: 0 = no educ, 1 = primary/vocational, 2 = high school, 3 = uni
    # code  99 = I prefer not to say
    income = case_when(income <= 10 ~ income, income == 99 ~ NA_real_),
    relig = case_when(relig <= 7 ~ relig, relig == 99 ~ NA_real_), 
    polit.lr = case_when(polit.lr <= 7 ~ polit.lr, polit.lr == 99 ~ NA_real_),  # -- DODAŁAM TO KODOWANIE
    # political party
    # recode political party into right- vs. left-wing
    polit.party.econ = case_when(polit.party %in% c(2, 7) ~ 1, # PO, Konfederecja
                                 polit.party %in% c(1, 3, 4, 5, 6) ~ 0, # Lewica, PSL, Polska2050, PIS, Solidarna
                                 TRUE ~ NA_real_), # other, don't know, don't vote 
    polit.party.cult = case_when(polit.party %in% c(3, 4, 5, 6, 7) ~ 1, # PSL, Polska 2050, PIS, Solidarna, Konfederacja
                                 polit.party %in% c(1, 2) ~ 0, # Lewica, PO
                                 TRUE ~ NA_real_),# other, don't know, don't vote 
    # add political party names 
    polit.party.name = case_when(polit.party == 1 ~ "1. Left", # 1 = Lewica (Razem, SLD, Wiosna)
                                 polit.party == 2 ~ "2. Civic Platform", # 2 = Platforma Obywatelska
                                 polit.party == 3 ~ "3. Polish Peasants Party", # 3 = Polskie Stronnictwo Ludowe
                                 polit.party == 4 ~ "4. Poland 2050", # 4 = Polska 2050 – Ruch Szymona Hołowni
                                 polit.party == 5 ~ "5. Law & Justice", # 5 = Prawo i Sprawiedliwość
                                 polit.party == 6 ~ "6. United Poland", # 6 = Solidarna Polska
                                 polit.party == 7 ~ "7. Conf. Liberty & Ind.",  # 7 = Konfederacja Wolność i Niepodległość (Konfederacja, KORWiN, Ruch Narodowy))
                                 polit.party == 97 |  polit.party == 98 |  polit.party == 99 ~ "IDK & Other"), # 97 = Inna (Jaka?), 98 = Nie wiem, 99 = Nie głosował(a)bym
    # ideology
    # Left vs. Right
    polit.lr3 = case_when(polit.lr %in% c(1, 2) ~ 0, #Left
                          polit.lr %in% c(3, 4, 5) ~ 0.5, #Moderate
                          polit.lr %in% c(6, 7) ~ 1, #Right
                          TRUE ~ NA_real_), # don't know 
    # moral progressivism vs. conservatism
    polit.cult3 = case_when(polit.cult %in% c(1, 2) ~ 0, # Progressive
                            polit.cult %in% c(3, 4, 5) ~ 0.5, # Moderate
                            polit.cult %in% c(6, 7) ~ 1), # Conservative
    # economic political ideology
    polit.econ3 = case_when(polit.econ %in% c(1, 2) ~ 0, # Social democracy
                            polit.econ %in% c(3, 4, 5) ~ 0.5, # Moderate
                            polit.econ %in% c(6, 7) ~ 1)) # Free-market

#------ reverse-score issue position and norms -----
# higher scores represent right-wing beliefs 

# position
data <- data %>% mutate(across(c(abort.pos,
                                 empl.pos,
                                 eu.pos,
                                 health.pos,
                                 parent.pos,
                                 business.pos,
                                 educ.pos,
                                 envir.pos,
                                 epid.pos,
                                 poor.pos,
                                 hous.pos,
                                 relcntry.pos,
                                 gay.pos,
                                 govspend.pos,
                                 union.pos,
                                 immigr.pos,
                                 priv.pos,
                                 race.pos,
                                 tax.pos,
                                 welf.pos,
                                 smallgov.pos,
                                 gender.pos,
                                 drug.pos,
                                 infr.pos,
                                 climate.pos,
                                 democr.pos,
                                 admineff.pos,
                                 cult.pos,
                                 agricult.pos,
                                 freed.pos,
                                 bigtech.pos,
                                 trans.pos, 
                                 constit.pos, 
                                 intern.pos,
                                 animal.pos),
                               .fns = ~ 8 - (.),
                               .names = "{.col}_r"))

# norm
data <- data %>% mutate(across(c(abort.norm,
                                 empl.norm,
                                 eu.norm,
                                 health.norm,
                                 parent.norm,
                                 business.norm,
                                 educ.norm,
                                 envir.norm,
                                 epid.norm,
                                 poor.norm,
                                 hous.norm,
                                 relcntry.norm,
                                 gay.norm,
                                 govspend.norm,
                                 union.norm,
                                 immigr.norm,
                                 priv.norm,
                                 race.norm,
                                 tax.norm,
                                 welf.norm,
                                 smallgov.norm,
                                 gender.norm,
                                 drug.norm,
                                 infr.norm,
                                 climate.norm,
                                 democr.norm,
                                 admineff.norm,
                                 cult.norm,
                                 agricult.norm,
                                 freed.norm,
                                 bigtech.norm,
                                 trans.norm, 
                                 constit.norm, 
                                 intern.norm,
                                 animal.norm),
                               .fns = ~ 8 - (.),
                               .names = "{.col}_r"))
# TO DO: add Israel (recode?)

#------ reverse-score responses for perceived distribution ----
# reverse score items for perceptions 
# (this is reversing it into the same name, which is not great but easier)

data <- data %>%
  rename(
    # for those issues we need to replace 12 -> 67
    abort.perc67 =  abort.perc_1,
    empl.perc67 =  empl.perc_1,
    eu.perc67 =  eu.perc_1,
    health.perc67 =  health.perc_1,
    parent.perc67 =  parent.perc_1,
    business.perc67 =  business.perc_1,
    educ.perc67 =  educ.perc_1,
    envir.perc67 =  envir.perc_1,
    epid.perc67 =  epid.perc_1,
    poor.perc67 =  poor.perc_1,
    hous.perc67 =  hous.perc_1,
    relcntry.perc67 =  relcntry.perc_1,
    gay.perc67 =  gay.perc_1,
    govspend.perc67 =  govspend.perc_1,
    union.perc67 =  union.perc_1,
    immigr.perc67 =  immigr.perc_1,
    priv.perc67 =  priv.perc_1,
    race.perc67 =  race.perc_1,
    tax.perc67 =  tax.perc_1,
    welf.perc67 =  welf.perc_1,
    smallgov.perc67 =  smallgov.perc_1,
    gender.perc67 =  gender.perc_1,
    drug.perc67 =  drug.perc_1,
    infr.perc67 =  infr.perc_1,
    climate.perc67 =  climate.perc_1,
    democr.perc67 =  democr.perc_1,
    admineff.perc67 =  admineff.perc_1,
    cult.perc67 =  cult.perc_1,
    agricult.perc67 =  agricult.perc_1,
    freed.perc67 =  freed.perc_1,
    bigtech.perc67 =  bigtech.perc_1,
    trans.perc67 =  trans.perc_1,
    constit.perc67 =  constit.perc_1,
    intern.perc67 =  intern.perc_1,
    animal.perc67 =  animal.perc_1,
    # rename vars 6-7 into 1-2
    abort.perc12 =  abort.perc_4,
    empl.perc12 =  empl.perc_4,
    eu.perc12 =  eu.perc_4,
    health.perc12 =  health.perc_4,
    parent.perc12 =  parent.perc_4,
    business.perc12 =  business.perc_4,
    educ.perc12 =  educ.perc_4,
    envir.perc12 =  envir.perc_4,
    epid.perc12 =  epid.perc_4,
    poor.perc12 =  poor.perc_4,
    hous.perc12 =  hous.perc_4,
    relcntry.perc12 =  relcntry.perc_4,
    gay.perc12 =  gay.perc_4,
    govspend.perc12 =  govspend.perc_4,
    union.perc12 =  union.perc_4,
    immigr.perc12 =  immigr.perc_4,
    priv.perc12 =  priv.perc_4,
    race.perc12 =  race.perc_4,
    tax.perc12 =  tax.perc_4,
    welf.perc12 =  welf.perc_4,
    smallgov.perc12 =  smallgov.perc_4,
    gender.perc12 =  gender.perc_4,
    drug.perc12 =  drug.perc_4,
    infr.perc12 =  infr.perc_4,
    climate.perc12 =  climate.perc_4,
    democr.perc12 =  democr.perc_4,
    admineff.perc12 =  admineff.perc_4,
    cult.perc12 =  cult.perc_4,
    agricult.perc12 =  agricult.perc_4,
    freed.perc12 =  freed.perc_4,
    bigtech.perc12 =  bigtech.perc_4,
    trans.perc12 =  trans.perc_4,
    constit.perc12 =  constit.perc_4,
    intern.perc12 =  intern.perc_4,
    animal.perc12 =  animal.perc_4)

names(data)
hist(data$abort.perc12)

# rename all remaining variables 
data <- data %>%  
  rename_at(vars(ends_with("perc_1")), ~str_replace(., "perc_1$", "perc12")) %>% # against
  rename_at(vars(ends_with("perc_2")), ~str_replace(., "perc_2$", "perc345")) %>% # moderate
  rename_at(vars(ends_with("perc_4")), ~str_replace(., "perc_4$", "perc67")) # pro

#------ TO DO: rescale position and norms between vars 0-1 ----
#TO DO: NOT SURE WE NEED IT 

# extremity pos - rekodowane normy ze skali 1-7 --> 1-4  
# extremity norm - rekodowane normy ze skali 1-7 --> 1-4 
#4 = 1 (moderate), 3 & 5 = 2,  2 & 6 = 3,  1 & 7 = 4 (extreme) 

extr_var_to_recode <- data %>%   # variables to modify
  select(ends_with(".pos") | ends_with(".pos_r") | 
           ends_with (".norm") | ends_with(".norm_r")) %>%
  names()

# recode responses as how far from the middle-point they are
data <- data %>%
  mutate(across(.cols = all_of(extr_var_to_recode),
                ~recode(., `1` = 4, `2` = 3, `3` = 2, `4` = 1, 
                        `5` = 2, `6` = 3, `7` = 4),
                .names = "{.col}extr"))

#	difference between own opinion and majority opinion perception
# TO DO: not sure we need this (leaving for now)
data <- data %>% mutate(misc.extrdiff = misc.posextr - misc.normextr,
                        terror.extrdiff = terror.posextr - terror.normextr,
                        agricult.extrdiff = agricult.posextr - agricult.normextr,
                        freed.extrdiff = freed.posextr - freed.normextr,
                        priv.extrdiff = priv.posextr - priv.normextr,
                        relcntry.extrdiff = relcntry.posextr - relcntry.normextr,
                        eu.extrdiff = eu.posextr - eu.normextr,
                        govinv.extrdiff = govinv.posextr - govinv.normextr,
                        law.extrdiff = law.posextr - law.normextr,
                        nation.extrdiff = nation.posextr - nation.normextr,
                        constit.extrdiff = constit.posextr - constit.normextr,
                        gay.extrdiff = gay.posextr - gay.normextr,
                        gun.extrdiff = gun.posextr - gun.normextr,
                        poor.extrdiff = poor.posextr - poor.normextr,
                        business.extrdiff = business.posextr - business.normextr,
                        infr.extrdiff = infr.posextr - infr.normextr,
                        moneypol.extrdiff = moneypol.posextr - moneypol.normextr,
                        union.extrdiff = union.posextr - union.normextr,
                        antimp.extrdiff = antimp.posextr - antimp.normextr,
                        climate.extrdiff = climate.posextr - climate.normextr,
                        democr.extrdiff = democr.posextr - democr.normextr,
                        race.extrdiff = race.posextr - race.normextr,
                        abort.extrdiff = abort.posextr - abort.normextr,
                        energy.extrdiff = energy.posextr - energy.normextr,
                        epid.extrdiff = epid.posextr - epid.normextr,
                        tax.extrdiff = tax.posextr - tax.normextr,
                        drug.extrdiff = drug.posextr - drug.normextr,
                        educ.extrdiff = educ.posextr - educ.normextr,
                        govspend.extrdiff = govspend.posextr - govspend.normextr,
                        intern.extrdiff = intern.posextr - intern.normextr,
                        admineff.extrdiff = admineff.posextr - admineff.normextr,
                        gender.extrdiff = gender.posextr - gender.normextr,
                        govinst.extrdiff = govinst.posextr - govinst.normextr,
                        tradv.extrdiff = tradv.posextr - tradv.normextr,
                        empl.extrdiff = empl.posextr - empl.normextr,
                        milit.extrdiff = milit.posextr - milit.normextr,
                        protect.extrdiff = protect.posextr - protect.normextr,
                        cult.extrdiff = cult.posextr - cult.normextr,
                        relig.extrdiff = relig.posextr - relig.normextr,
                        bigtech.extrdiff = bigtech.posextr - bigtech.normextr,
                        envir.extrdiff = envir.posextr - envir.normextr,
                        health.extrdiff = health.posextr - health.normextr,
                        hous.extrdiff = hous.posextr - hous.normextr,
                        smallgov.extrdiff = smallgov.posextr - smallgov.normextr,
                        trans.extrdiff = trans.posextr - trans.normextr,
                        welf.extrdiff = welf.posextr - welf.normextr,
                        immigr.extrdiff = immigr.posextr - immigr.normextr,
                        parent.extrdiff = parent.posextr - parent.normextr,
                        peace.extrdiff = peace.posextr - peace.normextr)         

# absolute difference = abs(pos - norm)
data <- data %>% 
  mutate(misc.extrabsdiff = abs(misc.posextr - misc.normextr),
         terror.extrabsdiff = abs(terror.posextr - terror.normextr),
         agricult.extrabsdiff = abs(agricult.posextr - agricult.normextr),
         freed.extrabsdiff = abs(freed.posextr - freed.normextr),
         priv.extrabsdiff = abs(priv.posextr - priv.normextr),
         relcntry.extrabsdiff = abs(relcntry.posextr - relcntry.normextr),
         eu.extrabsdiff = abs(eu.posextr - eu.normextr),
         govinv.extrabsdiff = abs(govinv.posextr - govinv.normextr),
         law.extrabsdiff = abs(law.posextr - law.normextr),
         nation.extrabsdiff = abs(nation.posextr - nation.normextr),
         constit.extrabsdiff = abs(constit.posextr - constit.normextr),
         gay.extrabsdiff = abs(gay.posextr - gay.normextr),
         gun.extrabsdiff = abs(gun.posextr - gun.normextr),
         poor.extrabsdiff = abs(poor.posextr - poor.normextr),
         business.extrabsdiff = abs(business.posextr - business.normextr),
         infr.extrabsdiff = abs(infr.posextr - infr.normextr),
         moneypol.extrabsdiff = abs(moneypol.posextr - moneypol.normextr),
         union.extrabsdiff = abs(union.posextr - union.normextr),
         antimp.extrabsdiff = abs(antimp.posextr - antimp.normextr),
         climate.extrabsdiff = abs(climate.posextr - climate.normextr),
         democr.extrabsdiff = abs(democr.posextr - democr.normextr),
         race.extrabsdiff = abs(race.posextr - race.normextr),
         abort.extrabsdiff = abs(abort.posextr - abort.normextr),
         energy.extrabsdiff = abs(energy.posextr - energy.normextr),
         epid.extrabsdiff = abs(epid.posextr - epid.normextr),
         tax.extrabsdiff = abs(tax.posextr - tax.normextr),
         drug.extrabsdiff = abs(drug.posextr - drug.normextr),
         educ.extrabsdiff = abs(educ.posextr - educ.normextr),
         govspend.extrabsdiff = abs(govspend.posextr - govspend.normextr),
         intern.extrabsdiff = abs(intern.posextr - intern.normextr),
         admineff.extrabsdiff = abs(admineff.posextr - admineff.normextr),
         gender.extrabsdiff = abs(gender.posextr - gender.normextr),
         govinst.extrabsdiff = abs(govinst.posextr - govinst.normextr),
         tradv.extrabsdiff = abs(tradv.posextr - tradv.normextr),
         empl.extrabsdiff = abs(empl.posextr - empl.normextr),
         milit.extrabsdiff = abs(milit.posextr - milit.normextr),
         protect.extrabsdiff = abs(protect.posextr - protect.normextr),
         cult.extrabsdiff = abs(cult.posextr - cult.normextr),
         relig.extrabsdiff = abs(relig.posextr - relig.normextr),
         bigtech.extrabsdiff= abs(bigtech.posextr - bigtech.normextr),
         envir.extrabsdiff = abs(envir.posextr - envir.normextr),
         health.extrabsdiff= abs(health.posextr - health.normextr),
         hous.extrabsdiff= abs(hous.posextr - hous.normextr),
         smallgov.extrabsdiff= abs(smallgov.posextr - smallgov.normextr),
         trans.extrabsdiff= abs(trans.posextr - trans.normextr),
         welf.extrabsdiff= abs(welf.posextr - welf.normextr),
         immigr.extrabsdiff = abs(immigr.posextr - immigr.normextr),
         parent.extrabsdiff= abs(parent.posextr - parent.normextr),
         peace.extrabsdiff = abs(peace.posextr - peace.normextr))

# check ditributions
hist(data$eu.posextr)

#------ calculate reliability and create indices ----
#political engagement 
#data <- data %>% 
#  mutate(across(c("polit.eng1", "polit.eng2", "polit.eng3"), 
#                .fns = ~scales::rescale(.),
#                .names = "{.col}_s"))

#polit.eng = c("polit.eng1_s", "polit.eng2_s", "polit.eng3_s")
#psych::alpha(data[polit.eng]) 
#data$polit.eng_s = rowMeans(data[polit.eng], na.rm = TRUE) 
#hist(data$polit.eng_s)
#paste0("Political engagement: M = ", round(mean(data$polit.eng_s, na.rm = TRUE), 2), ", SD = ", round(sd(data$pol.eng_s, na.rm = TRUE), 2))
#summary(data$polit.eng_s)

#---- Remove original ids ---- 
# and other not informative vars
data <- data %>% select(-id, -ResponseId,
                        -Progress, -Status, 
                        -StartDate, -EndDate, -Finished, 
                        -DistributionChannel, -Q_RecaptchaScore,
                        -UserLanguage)

#---- Save full data ----
write.csv2(data, "polarization study US full data.csv", row.names = FALSE)
#write.csv(data, "polarization study US full data.csv", row.names = FALSE)

#---- Filter data ----
nrow(data) #

# remove people who failed attention checks 
# att1
data <- data %>% filter(att.check1 == 1)
nrow(data) # N = 2027
# att2
data <- data %>% filter(att.memory1 == 1)
nrow(data) # N = 1864
# att3 -- TO DO: tu trzeba będzie jeszcze usunąć ew. spacje etc.
data <- data %>% filter(tolower(att.memory2) == "cat") 
nrow(data) # N = 1642
# att4
data <- data %>% filter(att.check.birthday == 2) 
nrow(data) # N = 1273

# remove people who didn't finish (?) 
# - if someone responded to birthday att check they responded to almost 
# full survey
#data1 <- data %>% filter(!is.na(household)) 

#---- Save clean data ----
write.csv2(data, "polarization study US clean data.csv", row.names = FALSE)
#write.csv(data, "polarization study US clean data.csv", row.names = FALSE)

#---- Create long data and issue means ----

# select positions, norm, perceived polarization, distributions
data.long <- data %>%
  dplyr::select(subj.id, 
                abort.pos_r, abort.norm_r, abort.polar, abort.perc12, abort.perc345, abort.perc67, 
                empl.pos_r, empl.norm_r, empl.polar, empl.perc12, empl.perc345, empl.perc67, 
                eu.pos_r, eu.norm_r, eu.polar, eu.perc12, eu.perc345, eu.perc67, 
                health.pos_r, health.norm_r, health.polar, health.perc12, health.perc345, health.perc67, 
                parent.pos_r, parent.norm_r, parent.polar, parent.perc12, parent.perc345, parent.perc67, 
                business.pos_r, business.norm_r, business.polar, business.perc12, business.perc345, business.perc67, 
                educ.pos_r, educ.norm_r, educ.polar, educ.perc12, educ.perc345, educ.perc67, 
                envir.pos_r, envir.norm_r, envir.polar, envir.perc12, envir.perc345, envir.perc67, 
                epid.pos_r, epid.norm_r, epid.polar, epid.perc12, epid.perc345, epid.perc67, 
                poor.pos_r, poor.norm_r, poor.polar, poor.perc12, poor.perc345, poor.perc67, 
                hous.pos_r, hous.norm_r, hous.polar, hous.perc12, hous.perc345, hous.perc67, 
                relcntry.pos_r, relcntry.norm_r, relcntry.polar, relcntry.perc12, relcntry.perc345, relcntry.perc67, 
                israel.pos, israel.norm, israel.polar, israel.perc12, israel.perc345, israel.perc67, 
                gay.pos_r, gay.norm_r, gay.polar, gay.perc12, gay.perc345, gay.perc67, 
                govspend.pos_r, govspend.norm_r, govspend.polar, govspend.perc12, govspend.perc345, govspend.perc67, 
                union.pos_r, union.norm_r, union.polar, union.perc12, union.perc345, union.perc67, 
                immigr.pos_r, immigr.norm_r, immigr.polar, immigr.perc12, immigr.perc345, immigr.perc67, 
                priv.pos_r, priv.norm_r, priv.polar, priv.perc12, priv.perc345, priv.perc67, 
                race.pos_r, race.norm_r, race.polar, race.perc12, race.perc345, race.perc67, 
                tax.pos_r, tax.norm_r, tax.polar, tax.perc12, tax.perc345, tax.perc67, 
                welf.pos_r, welf.norm_r, welf.polar, welf.perc12, welf.perc345, welf.perc67, 
                smallgov.pos_r, smallgov.norm_r, smallgov.polar, smallgov.perc12, smallgov.perc345, smallgov.perc67, 
                gender.pos_r, gender.norm_r, gender.polar, gender.perc12, gender.perc345, gender.perc67, 
                drug.pos_r, drug.norm_r, drug.polar, drug.perc12, drug.perc345, drug.perc67, 
                infr.pos_r, infr.norm_r, infr.polar, infr.perc12, infr.perc345, infr.perc67, 
                climate.pos_r, climate.norm_r, climate.polar, climate.perc12, climate.perc345, climate.perc67, 
                democr.pos_r, democr.norm_r, democr.polar, democr.perc12, democr.perc345, democr.perc67, 
                admineff.pos_r, admineff.norm_r, admineff.polar, admineff.perc12, admineff.perc345, admineff.perc67, 
                cult.pos_r, cult.norm_r, cult.polar, cult.perc12, cult.perc345, cult.perc67, 
                agricult.pos_r, agricult.norm_r, agricult.polar, agricult.perc12, agricult.perc345, agricult.perc67, 
                freed.pos_r, freed.norm_r, freed.polar, freed.perc12, freed.perc345, freed.perc67, 
                bigtech.pos_r, bigtech.norm_r, bigtech.polar, bigtech.perc12, bigtech.perc345, bigtech.perc67, 
                trans.pos_r, trans.norm_r, trans.polar, trans.perc12, trans.perc345, trans.perc67, 
                constit.pos_r, constit.norm_r, constit.polar, constit.perc12, constit.perc345, constit.perc67, 
                intern.pos_r, intern.norm_r, intern.polar, intern.perc12, intern.perc345, intern.perc67, 
                gun.pos, gun.norm, gun.polar, gun.perc12, gun.perc345, gun.perc67, 
                law.pos, law.norm, law.polar, law.perc12, law.perc345, law.perc67, 
                tradv.pos, tradv.norm, tradv.polar, tradv.perc12, tradv.perc345, tradv.perc67, 
                misc.pos, misc.norm, misc.polar, misc.perc12, misc.perc345, misc.perc67, 
                relig.pos, relig.norm, relig.polar, relig.perc12, relig.perc345, relig.perc67, 
                govinv.pos, govinv.norm, govinv.polar, govinv.perc12, govinv.perc345, govinv.perc67, 
                milit.pos, milit.norm, milit.polar, milit.perc12, milit.perc345, milit.perc67, 
                energy.pos, energy.norm, energy.polar, energy.perc12, energy.perc345, energy.perc67, 
                nation.pos, nation.norm, nation.polar, nation.perc12, nation.perc345, nation.perc67, 
                govinst.pos, govinst.norm, govinst.polar, govinst.perc12, govinst.perc345, govinst.perc67, 
                protect.pos, protect.norm, protect.polar, protect.perc12, protect.perc345, protect.perc67, 
                terror.pos, terror.norm, terror.polar, terror.perc12, terror.perc345, terror.perc67, 
                moneypol.pos, moneypol.norm, moneypol.polar, moneypol.perc12, moneypol.perc345, moneypol.perc67, 
                antimp.pos, antimp.norm, antimp.polar, antimp.perc12, antimp.perc345, antimp.perc67, 
                peace.pos, peace.norm, peace.polar, peace.perc12, peace.perc345, peace.perc67, 
                animal.pos_r, animal.norm_r, animal.polar, animal.perc12, animal.perc345, animal.perc67)

# TO DO: rescale vars 0-1 (so interpretation is easier)

# select and restructure dataset with other issues
data.long <- data.long %>% 
  #select(subj.id, ends_with("_s")) %>% TO DO: use later (only rescaled vars)
  # restructure data
  gather(key = "issue.question", value = "resp", -subj.id) %>% # create long format 
  # remove _r from var names
  mutate(issue.question = gsub("_r", "", issue.question)) %>% 
  filter(!is.na(resp)) %>%  # remove observations with missing data 
  separate(issue.question, into = c("issue", "question"), sep = c("\\.")) %>% # separate var names (using a dot)
  spread(question, resp) %>%
  select(subj.id, issue, pos, norm, polar, perc12, perc345, perc67, 
         everything())

# calculate SDs of perceived distributions
data.long <- data.long %>% 
  mutate(perc.m = (1.5 * perc12 + 4 * perc345 + 6.5 * perc67)/100,
    perc.sd = sqrt(((1.5 - perc.m)^2 * perc12 + 
                   (4 - perc.m)^2 * perc345 +
                   (6.5 - perc.m)^2 * perc67)/99))

#---- calculate means and sds for position, norm, polarization 
issue.means <- data.long %>% 
  group_by(issue) %>%
  summarize(n = n(),
            pos.m = mean(pos, na.rm = TRUE),
            pos.sd = sd(pos, na.rm = TRUE),
            pos.se = pos.sd/(sqrt(n)),
            norm.m = mean(norm, na.rm = TRUE),
            norm.sd = sd(norm, na.rm = TRUE),
            norm.se = norm.sd/(sqrt(n)),
            polar.m = mean(polar, na.rm = TRUE),
            polar.sd = sd(polar, na.rm = TRUE),
            polar.se = polar.sd/(sqrt(n))) %>%
  ungroup()

# add means to the main data
data.long <- data.long %>% left_join(issue.means, by = "issue")

# center responses
data.long <- data.long %>% 
  mutate(pos_c = pos - pos.m,
         norm_c = norm - pos.m, # centered on "actual" norm 
         norm_c_abs = abs(norm - pos.m))

data.long %>% group_by(issue) %>%
  summarize(n = n())

#---- Save long data ----
write.csv2(data.long, "polarization study US long data.csv", row.names = FALSE)
#write.csv(data.long, "polarization study US long data.csv", row.names = FALSE)
write.csv2(issue.means, "polarization study US issue means.csv", row.names = FALSE)
