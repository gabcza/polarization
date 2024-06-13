#--- Info -----
# Goal: Analysis of polarization (study 1) data, pre-reg: https://osf.io/xw7sp
# 
# wriyten 25-04-2023 by Gabriela & Iwona
#---------------------------------------------------------------------------------------------------------------------

#---- Load packages ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)
library(lme4) # for multilevel analysis
library(lmerTest) # for multilevel analysis
library(ggeffects) # for predictions from mlm analysis
library(directlabels) # for labels in plots
library(sjPlot) 
library(ggrepel) # for labels in plots
options(scipen = 999) # non-scientific notation

#---- Load data ----
# use code 01 or load csv
#data <- read.csv2("polarization study 1 clean data.csv")
#data.long <- read.csv2("polarization study 1 long data.csv")
#issue.means <- read.csv2("polarization study 1 issue means.csv")

#---- Plots ----
#---- plot histograms
# positions
data.long %>%
  ggplot(aes(pos, fill = issue)) + 
  geom_histogram(binwidth = 1, colour = "white") + 
  #facet_wrap(~issue, scales = "free") + 
  facet_wrap(~issue, scales = "free", ncol = 7) + 
  labs(x = "Issue position (coded left-right)") +
  theme_classic() + theme(legend.position = "none")

# norms
data.long %>%
  ggplot(aes(norm, fill = issue)) + 
  geom_histogram(binwidth = 1, colour = "white") + 
  #facet_wrap(~issue, scales = "free") + 
  facet_wrap(~issue, scales = "free", ncol = 7) + 
  labs(x = "Meta-perceptions (coded left-right)") +
  theme_classic() + theme(legend.position = "none")

# perceived polarization
data.long %>% filter(resp4 < 20) %>%
  ggplot(aes(polar, fill = issue)) + 
  geom_histogram(binwidth = 1, colour = "white") + 
  #facet_wrap(~issue, scales = "free") + 
  facet_wrap(~issue, scales = "free", ncol = 7) + 
  labs(x = "Perceived polarization") +
  theme_classic() + theme(legend.position = "none")

#---- plot means and sd 
# means for positions
plot.pos <- issue.means %>%
  ggplot(aes(x=reorder(issue, pos.m), y = pos.m)) +
  geom_point(size = 3, colour = "violetred3") + 
  theme_classic() +
  geom_segment(aes(x = issue, xend = issue, y = 1, yend = pos.m), colour = "violetred3")+
  labs(y= NULL, x= NULL, title = "Average issue positions")+
  #scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.50,0.75,1))+ # TO DO: use this after rescaling vars
  scale_y_continuous(limits = c(1,7), breaks = c(1,2,3,4,5,6,7))+ 
  geom_label(aes(issue, pos.m + 0.25, label = signif(pos.m, 2)), colour = "violetred3", size = 2)+
  theme(axis.text.x = element_text(size = 10, colour = "black"), 
        plot.title = element_text(size = 14), 
        axis.text.y = element_text(size = 10, colour = "black")) +
  coord_flip()
plot.pos

# means for norms (order jak w position)
plot.norm <- issue.means %>%
  ggplot(aes(x=reorder(issue, pos.m), y = norm.m)) +
  geom_point(size = 3, colour = "violetred4") + 
  theme_classic() +
  geom_segment(aes(x = issue, xend = issue, y = 1, yend = norm.m), colour = "violetred4")+
  labs(y= NULL, x= NULL, title = "Meta-perceptions")+
  #scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.50,0.75,1))+ 
  scale_y_continuous(limits = c(1,7), breaks = c(1,2,3,4,5,6,7))+ 
  geom_label(aes(issue, norm.m + 0.25, label = signif(pos.m, 2)), colour = "violetred3", size = 2)+
  theme(axis.text.x = element_text(size = 10, colour = "black"), 
        plot.title = element_text(size = 14), 
        axis.text.y = element_text(size = 10, colour = "black")) +
  coord_flip()
plot.norm
gridExtra::grid.arrange(plot.pos, plot.norm, nrow = 1)

# difference between actual and perceived issue positions
plot.norm1 <- issue.means %>% select(issue, pos.m, norm.m) %>%
  ggplot() +
  geom_segment(aes(x=reorder(issue, pos.m), xend = issue, y = pos.m, yend = norm.m), color="grey") +
  geom_point(aes(x = issue, y = pos.m), color= "violetred2", size = 2) +
  geom_point(aes(x = issue, y = norm.m), color= "violetred4", size = 2) +
  #geom_label(aes(issue, pos.m+0.50, label = signif(pos.m,2)), colour = "violetred3", size = 2)+
  #geom_label(aes(issue, norm.m-0.50, label = signif(norm.m,2)), colour = "violetred4", size = 2)+
  #coord_flip(ylim = c(0,1))+
  scale_y_continuous(limits = c(1,7), breaks = c(1,2,3,4,5,6,7))+ 
  coord_flip() +
  labs(y = "", x = "", title = "Actual positions (pink) and meta-perceptions (purple)")+
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 10, colour = "black"), 
        plot.title = element_text(size = 14), 
        axis.text.y = element_text(size = 8, colour = "black"))
plot.norm1 

# means for perceived polarization 
plot.polar <- issue.means %>%
  ggplot(aes(x = reorder(issue, polar.m), y = polar.m)) +
  geom_point(size = 3, colour = "violetred4") + 
  theme_classic() +
  geom_segment(aes(x = issue, xend = issue, y = 1, yend = polar.m), colour = "violetred4")+
  labs(y= NULL, x= NULL, title = "Perception of polarization")+
  #scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.50,0.75,1))+ # TO DO: use this after rescaling vars
  scale_y_continuous(limits = c(1,7), breaks = c(1,2,3,4,5,6,7))+ 
  geom_label(aes(issue, polar.m + 0.25, label = signif(polar.m, 2)), colour = "violetred4", size = 2)+
  theme(axis.text.x = element_text(size = 10, colour = "black"), 
        plot.title = element_text(size = 14), 
        axis.text.y = element_text(size = 10, colour = "black")) +
  coord_flip()
plot.polar

# sd for positions (order as in perceived polarization)
plot.sd <- issue.means %>%
  ggplot(aes(x=reorder(issue, polar.m), y = pos.sd)) +
  geom_point(size = 3, colour = "violetred3") + 
  theme_classic() +
  geom_segment(aes(x = issue, xend = issue, y = 1, yend = pos.sd), colour = "violetred3")+
  labs(y= NULL, x= NULL, title = "SD of issue positions")+
  #scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.50,0.75,1))+ # TO DO: use this after rescaling vars
  scale_y_continuous(limits = c(1,3), breaks = c(1,2,3))+ 
  geom_label(aes(issue, pos.sd + 0.10, label = signif(pos.sd, 2)), colour = "violetred3", size = 2)+
  theme(axis.text.x = element_text(size = 10, colour = "black"), 
        plot.title = element_text(size = 14), 
        axis.text.y = element_text(size = 8, colour = "black")) +
  coord_flip()
plot.sd


#---- Q1: How accurate are meta-perceptions? ----
# Q1. How accurate are the perceptions of average societal position (meta-perceptions) within and across multiple political issues? 

# add info on how many times someone picked rep 4
x <- data.long %>% filter(polar == 4) %>% filter(norm == 4) %>% #filter(pos == 4) %>%
  group_by(subj.id) %>% summarize(n = n()) %>% rename(resp4 = n) #%>% filter(n > 10)
data.long <- data.long %>% left_join(x, by = "subj.id")
data <- data %>% left_join(x, by = "subj.id")

# null model
m0.norm <- lmer(data = data.long, norm ~ 1 + 
             (1|subj.id) + (1|issue))
summary(m0.norm)
# ICC
VarCorr(m0.norm) %>% # get variance components (these are SDs) 
  as_tibble() %>%
  mutate(icc=vcov/sum(vcov)) %>%
  dplyr::select(grp, icc)
# subj 4%, issue 20%

# non-centerd DVs, issue mean as predictor
m1.norm <- lmer(data = data.long, norm ~ 1 + pos.m +
                  (1|subj.id) + (1|issue))
summary(m1.norm)

# centered DVs
m2.norm <- lmer(data = data.long, norm_c ~ 1 +
                  (1|subj.id) + (1|issue))
summary(m2.norm)
# ICC
VarCorr(m2.norm) %>% # get variance components (these are SDs) 
  as_tibble() %>%
  mutate(icc=vcov/sum(vcov)) %>%
  dplyr::select(grp, icc)
# subj 4%, issue 2%
# shows random effects
d.m2.norm <- get_model_data(m2.norm, type = "re")[[2]]
d.m2.norm <- d.m2.norm %>% arrange(-estimate)
d.m2.norm %>% mutate(estimate = round(estimate,2)) %>% select(term, estimate)
# plot random effects + fixed intercept
d.m2.norm <- d.m2.norm %>% 
  mutate(estimate.f = fixef(m2.norm)[[1]] + estimate, # add value of fixed effect so it shows not deviation from it but actual value (you can check it using: coef(m4g)[[1]][c(1,10)])
         conf.low.f = fixef(m2.norm)[[1]] + conf.low,
         conf.high.f = fixef(m2.norm)[[1]] + conf.high) %>%
  mutate(negative = case_when(conf.high.f < 0 ~ "negative", 
                              conf.low.f > 0 ~ "positive",
                              TRUE ~ "ns")) #%>% # assing 0 when upper CI is below 
d.m2.norm %>% 
  ggplot(aes(x = reorder(term, estimate.f), y = estimate.f, col = negative)) +
  geom_point(show.legend = FALSE) + 
  geom_segment(aes(xend = term, y = conf.low.f, yend = conf.high.f), show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "black") +
  labs(title = "Bias in meta-perception",
       x = "Issue",
       y = "Bias") + 
  scale_color_manual(values=c("blue", "grey60", "red")) +
  #scale_x_discrete(label = function(x) substr(x, start = 6, stop = 20)) + #length(x))) +# update 24-10-2023 for showing only country names
  coord_flip() +
  theme_classic(base_size = 14) #+ 

# centered DVs
m3.norm <- lmer(data = data.long, norm_c_abs ~ 1 +
                  (1|subj.id) + (1|issue))
summary(m3.norm)
# ICC
VarCorr(m3.norm) %>% # get variance components (these are SDs) 
  as_tibble() %>%
  mutate(icc=vcov/sum(vcov)) %>%
  dplyr::select(grp, icc)
# subj 24%, issue 2%
# shows random effects
d.m3.norm <- get_model_data(m3.norm, type = "re")[[2]]
d.m3.norm <- d.m3.norm %>% arrange(-estimate)
d.m3.norm %>% mutate(estimate = round(estimate,2)) %>% select(term, estimate)
# plot random effects + fixed intercept
d.m3.norm <- d.m3.norm %>% 
  mutate(estimate.f = fixef(m3.norm)[[1]] + estimate, # add value of fixed effect so it shows not deviation from it but actual value (you can check it using: coef(m4g)[[1]][c(1,10)])
         conf.low.f = fixef(m3.norm)[[1]] + conf.low,
         conf.high.f = fixef(m3.norm)[[1]] + conf.high) %>%
  mutate(negative = case_when(conf.high.f < 0 ~ "negative", 
                              conf.low.f > 0 ~ "positive",
                              TRUE ~ "ns")) #%>% # assing 0 when upper CI is below 
d.m3.norm %>% 
  ggplot(aes(x = reorder(term, estimate.f), y = estimate.f, col = negative)) +
  geom_point(show.legend = FALSE) + 
  geom_segment(aes(xend = term, y = conf.low.f, yend = conf.high.f), show.legend = FALSE) +
  geom_hline(yintercept = 1.22, color = "black") +
  labs(title = "Bias in meta-perception",
       x = "Issue",
       y = "Bias") + 
  scale_color_manual(values=c("blue", "grey60", "red")) +
  #scale_x_discrete(label = function(x) substr(x, start = 6, stop = 20)) + #length(x))) +# update 24-10-2023 for showing only country names
  coord_flip() +
  theme_classic(base_size = 14) #+ 

# summary of the models
tab_model(m0.norm, m1.norm, m2.norm, m3.norm)

#---- Q2: How accurate are the perceptions of polarization? ----
# null model
m0.polar <- lmer(data = data.long, polar ~ 1 + 
                  (1|subj.id) + (1|issue))
summary(m0.polar)
# ICC
VarCorr(m0.polar) %>% # get variance components (these are SDs) 
  as_tibble() %>%
  mutate(icc=vcov/sum(vcov)) %>%
  dplyr::select(grp, icc)
# subj 24%, issue 3%

# non-centerd DVs, issue mean as predictor
m1.polar <- lmer(data = data.long, polar ~ 1 + pos.sd +
                  (1|subj.id) + (1|issue))
summary(m1.polar)

tab_model(m0.polar, m1.polar)

#---- Q3. What is the relationship between perceived polarization and perceived distribution of opinions? ----
# Under what distribution people believe an issue is polarized?

# null model
m0.perc <- lmer(data = data.long, perc.sd ~ 1 + 
                   (1|subj.id) + (1|issue))
summary(m0.perc)
# ICC
VarCorr(m0.perc) %>% # get variance components (these are SDs) 
  as_tibble() %>%
  mutate(icc=vcov/sum(vcov)) %>%
  dplyr::select(grp, icc)
# subj 55%, issue 1%

# perceived polarization as a predictor
m1.perc <- lmer(data = data.long, perc.sd ~ 1 + polar +
                  (1|subj.id) + (1|issue))
summary(m1.perc)

# add RS
m1a.perc <- lmer(data = data.long, perc.sd ~ 1 + polar +
                  (1|subj.id) + (1 + polar|issue))
summary(m1a.perc)

# issue sd as predictor
m2.perc <- lmer(data = data.long, perc.sd ~ 1 + pos.sd +
                  (1|subj.id) + (1|issue))
summary(m2.perc)

tab_model(m0.perc, m1.perc, m1a.perc, m2.perc)

#---- Analysis as in NHB paper ----

