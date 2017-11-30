# anlaysis 2

# warning: if you publish this, you should double check the calculation of symptoms/
library(stats); library(tidyverse); options(scipen=9e4)
data <- readRDS("data/NSDUH_for_drug_policy_allyears.rds") %>% 
  mutate(plyr::mapvalues(x = t, from=c(1,2), to=c("2002-2004", "2013-2015")),
         depndmrj = factor(depndmrj),
         use_recency = factor(use_recency))


#  First Step: Compute Trends and Charts ----------------------------------


# analysis 1 - Prevalence of Dependence
levels(data$depndmrj)
# DND users only
dependence_rates <- data %>% # (even bigger decline)
  # filter(use_recency %in% c("DND")) %>% 
  group_by(year, t, use_recency) %>% 
  dplyr::summarise(diagnosis_rate = weighted.mean(depndmrj=="Yes", wt)) %>%
  rbind(data %>% 
          mutate(use_recency='Population') %>%
          group_by(year, t, use_recency) %>%
          dplyr::summarise(diagnosis_rate  = weighted.mean(depndmrj=="Yes", wt)))

# Rates by year
dependence_rates %>% spread(use_recency, diagnosis_rate)

# Rates by time period
dependence_rates %>%
  group_by(t, use_recency) %>%
  dplyr::summarise(diagnosis_rate= mean(diagnosis_rate)) %>% 
  spread(use_recency, diagnosis_rate)

dependence_rates %>%
  filter(use_recency=='DND') %>%
  ggplot(aes(x=year, y=diagnosis_rate)) + 
  geom_line() + geom_point(size=2) + 
  scale_y_continuous(labels=scales::percent_format(), limits=c(0, .3)) +
  scale_x_continuous(breaks=c(2002, 2005, 2010, 2015)) +
  labs(title="Marijuana Dependence",
       subtitle="Prevalence Among Daily/Near-Daily Users",
       x='', y="") +
  ggthemes::theme_base() +
  theme(plot.margin=margin(.4,1,0,0, "cm"))


# Analysis 2 - Prevalence of Symptoms

# DND Only
symptom_rates <- data %>% 
  filter(use_recency=="DND") %>%  
  group_by(year, use_recency) %>%
  dplyr::summarise(
    spent_time_getting_OR_getting_over = weighted.mean(spent_time_getting_OR_getting_over == "Yes", wt, na.rm=T),
    kept_limits_AND_failed = weighted.mean(kept_limits_AND_failed == "Yes", wt, na.rm=T),
    need_more_OR_less_effect = weighted.mean(need_more_OR_less_effect == "Yes", wt, na.rm=T),
    unable_cut_down =  weighted.mean(unable_cut == "Yes", wt, na.rm=T),
    mental_OR_physical_problems_AND_use = weighted.mean(mental_OR_physical_problems_AND_use == "Yes", wt, na.rm=T),
    stop_important_activities = weighted.mean(stop_important_activities == "Yes", wt, na.rm=T)) %>%
  gather(symptom, rate, spent_time_getting_OR_getting_over:stop_important_activities) %>%
  dplyr::mutate(symptom = factor(ifelse(symptom=="spent_time_getting_OR_getting_over", "Lots of Time\nGetting, Using, or\nGetting Over Marijuana", 
                          ifelse(symptom =="kept_limits_AND_failed", "Set Limits on Use\nbut Failed to Keep Them",
                                 ifelse(symptom=="need_more_OR_less_effect", "Needed More for\nSame Effect",
                                        ifelse(symptom=="unable_cut_down", "Attempted to Reduce\nUse but Failed",
                                               ifelse(symptom=="mental_OR_physical_problems_AND_use", 
                                                      "Emotional, Mental, or\nPhysical Problems and\nContinued Use", 
                                                      "Reduced or Gave\nUp Important\nActivities")))))),
         symptom = factor(symptom, 
                           levels=c("Lots of Time\nGetting, Using, or\nGetting Over Marijuana", 
                                    "Needed More for\nSame Effect",
                                    "Attempted to Reduce\nUse but Failed",
                                    "Emotional, Mental, or\nPhysical Problems and\nContinued Use", 
                                    "Reduced or Gave\nUp Important\nActivities",
                                    "Set Limits on Use\nbut Failed to Keep Them")))

symptom_rates %>%
  ggplot(aes(x=year, y=rate, color=symptom, group=symptom)) + geom_line() + geom_point(size=3) + 
  scale_y_continuous(labels=scales::percent_format(), limits=c(0, .8)) +
    scale_x_continuous(breaks=c(2002, 2005, 2010, 2015)) +
  labs(x="", title="Dependence Symptoms",
       subtitle="Prevalence Among Daily/Near-Daily Users",
       y="", color="  Symptom") +
  ggthemes::theme_base()+
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  guides(color=guide_legend(keyheight=1.5,   default.unit="cm"))
  


# Second Step - Compute Significance --------------------------------------
library(stargazer)
# If you'd like to test for significance of a trend over time, rather than @ two points,
# follow these instructions. But I tried and it showed insignificant!
# https://www.r-bloggers.com/statistically-significant-trends-with-multiple-years-of-complex-survey-data/
# Similar to the 2014 public use file, the 2015 public use file variance estimation stratum 
# variable VESTR was aggregated into 50 pseudo-strata due to sample redesign since the 2014 NSDUH, 
# whereas in 2013 and prior years, there were 60 pseudo-strata.
# construct a complex sample survey design object
# stacking multiple years and accounting for `year` in the nested strata
# remove unnecessary variables, keeping only the complex sample survey design columns
# plus independent/dependent variables to be used in the regression analyses

library(survey)
options( survey.lonely.psu = "adjust" )

# Declare survey design object
des <- svydesign(
  id = ~verep, 
  strata = ~ interaction(vestr, year),
  weights = ~wt, 
  data=data %>%
    mutate(depndmrj = factor(depndmrj, levels=c("No/Unknown", "Yes")),
           dnd = factor(use_recency%in%'DND')), 
  nest = TRUE)

# Add this (LaTeX) table to the word doc
# Now run a model on DND users that adjusts for covariates
dndonly <- 
  svyglm(
    I( depndmrj == levels(depndmrj)[2] ) ~ t + newrace2 + catag3 + educcat2 + irsex,
    design = des %>% subset(use_recency=='DND'),
    family = quasibinomial
  )
summary( dndonly )

# Now do the same but for each symptom; grouped by p-value
# p < -.001

reg_activities <- svyglm(
  I(stop_important_activities == levels(stop_important_activities)[1] ) ~ t + newrace2 + catag3 + educcat2 + irsex,
  design = des %>% subset(use_recency=='DND'),
  family = quasibinomial)
reg_activities %>% summary

reg_problems <- svyglm(
  I(mental_OR_physical_problems_AND_use == levels(mental_OR_physical_problems_AND_use)[1] ) ~ t +
    newrace2 + catag3 + educcat2 + irsex,
  design = des %>% subset(use_recency=='DND'),
  family = quasibinomial)
reg_problems%>% summary

stargazer(reg_activities, reg_problems, 
          column.labels=c("Activities","Problems"), align=TRUE)


# p < .05
svyglm(
  I(kept_limits_AND_failed == levels(kept_limits_AND_failed)[1] ) ~ t + newrace2 + catag3 + educcat2 + irsex,
  design = des %>% subset(use_recency=='DND'),
  family = quasibinomial) %>% summary

svyglm(
  I(unable_cut == levels(unable_cut)[1] ) ~ t + newrace2 + catag3 + educcat2 + irsex,
  design = des %>% subset(use_recency=='DND'),
  family = quasibinomial) %>% summary

svyglm(
  I(spent_time_getting_OR_getting_over == levels(spent_time_getting_OR_getting_over)[1] ) ~ t + newrace2 + catag3 + educcat2 + irsex,
  design = des %>% subset(use_recency=='DND'),
  family = quasibinomial) %>% summary

# p>0.1
svyglm(
  I(need_more_OR_less_effect == levels(need_more_OR_less_effect)[1] ) ~ t + newrace2 + catag3 + educcat2 + irsex,
  design = des %>% subset(use_recency=='DND'),
  family = quasibinomial) %>% summary

