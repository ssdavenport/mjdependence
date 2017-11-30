# Cleaning data.
library(tidyverse)

data <- readRDS("data/NSDUH_for_drug_policy_raw.rds")

# Rename variables --------------------------------------------------------

# MRJLOTTM
# During the past 12 months, was there a month or more when you spent a lot of your time getting or using marijuana or hashish?
  
# MRJGTOVR
# During the past 12 months, was there a month or more when you spent a lot of your time getting over hashish you used?

# MRJLIMIT
# During the past 12 months, did you try to set limits on how often or how much marijuana or hashish you would use?

# MRJKPLMT
# Were you able to keep to the limits you set, or did you often use marijuana or hashish more than you intended to?

# MRJNDMOR
# During the past 12 months, did you need to use more marijuana or hashish than you used to in order to get the effect you wanted?

# MRJLSEFX
# During the past 12 months, did you notice that using the same amount of marijuana or hashish had less effect on you than it used to?

# MRJCUTDN
# During the past 12 months, did you want to or try to cut down or stop using marijuana or hashish?

# MRJCUTEV
# During the past 12 months, were you able to cut down or stop using marijuana or hashish every time you wanted to or tried to?

# MRJEMOPB
# During the past 12 months, did you have any problems with your emotions, nerves, or mental health that were probably caused or made worse by your use of marijuana or hashish?

# MRJEMCTD
# Did you continue to use marijuana or hashish even though you thought it was causing you to have problems with your emotions, nerves, or mental health?

# MRJPHLPB
# During the past 12 months, did you have any physical health problems that were probably caused or made worse marijuana or hashish?

# MRJPHCTD
# Did you continue to use marijuana or hashish even though you thought it was causing you to have physical problems?

# MRJLSACT
# During the past 12 months, did using marijuana or hashish cause you to give up or spend less time doing these types of important activities?

# MRJSERPB
# During the past 12 months, did using marijuana or hashish cause you to have serious problems like this either at home, work, or school?

# MRJPDANG
# During the past 12 months, did you regularly use marijuana or hashish and then do something where using marijuana or hashish might have put you in physical danger?

# MRJLAWTR
# During the past 12 months, did using marijuana or hashish cause you to do things that repeatedly got you in trouble with the law?

# MRJFMFPB
# During the past 12 months, did you have any problems with family or friends that were probably caused by your use of marijuana or hashish?

# MRJFMCTD
# Did you continue to use marijuana or hashish even though you thought it caused problems with family or friends?

data <- data %>% 
  dplyr::rename(
                # Dependence criteria
                  # Solo
                stop_important_activities = mrjlsact,
                
                  # compound: either one
                spent_time_getting_using = mrjlottm,
                spent_time_over = mrjgtovr,
                
                  # compound: either one qualifies
                need_more = mrjndmor, 
                less_effect = mrjlsefx,
                
                  # compound: keep and set (only not-keep is needed)
                keep_limits = mrjkplmt,
                set_limits = mrjlimit,
                
                  # compound: Want and able (only not-able is needed)
                want_cut_down = mrjcutdn,
                able_cut_down = mrjcutev,
               
                  # compound: only use despite qualifies
                mental_problems = mrjemopb,
                use_despite_mental_problems = mrjemctd,
                
                # Abuse criteria
                   # compound
                physical_problems = mrjphlpb,
                use_despite_physical_problems = mrjphctd,
                   # compound
                family_problems = mrjfmfpb,
                use_despite_family_problems = mrjfmctd,
                
                serious_problems = mrjserpb,
                dangerous_activities = mrjpdang,
                legal_trouble = mrjlawtr,
               wt=analwt_c)



# Add additional columns --------------------------------------------------

# Add columns to easily show user type.
data <- data %>% dplyr::mutate(
  use_recency = ifelse(
    irmjrc %in% levels(irmjrc)[4], "Never", ifelse(
      irmjrc %in% levels(irmjrc)[3], "Lifetime", ifelse(
        irmjrc %in% levels(irmjrc)[2], "PY", ifelse(
          irmjrc %in% levels(irmjrc)[1] & mjday30a <= 20, "PM", ifelse(
            mjday30a > 20, "DND", NA))))),
  ever = use_recency %in% c("Lifetime", "PY", "PM", "DND"),
  py = use_recency %in% c("PY", "PM", "DND"),
  pm = use_recency %in% c("PM", "DND"),
  dnd = use_recency == "DND")

# Clean the dependence criteria to remove NAs (yes is yes, DK is no)



# Make new variables for compound symptom-criteria
data <- data %>% 
  mutate(family_problems_AND_use = ifelse(
    use_despite_family_problems %in% "(1) Yes", "Yes", ifelse(
      use_despite_family_problems %in% "(2) No", "No", NA)) %>%
      factor(levels=c("Yes", "No")))

data <- data %>% 
  mutate(mental_problems_AND_use = ifelse(
    use_despite_mental_problems %in% "(1) Yes", "Yes", ifelse(
      use_despite_mental_problems %in% "(2) No", "No", NA)) %>%
      factor(levels=c("Yes", "No")))

data <- data %>% 
  mutate(physical_problems_AND_use = ifelse(
    use_despite_physical_problems %in% "(1) Yes", "Yes", ifelse(
      !use_despite_physical_problems %in% "(2) No", "No", NA)) %>%
      factor(levels=c("Yes", "No")))


# Dependence criteria: Unable to keep limits
data <- data %>% 
  mutate(kept_limits_AND_failed = ifelse(
    keep_limits %in% "(2) Often used more than intended", "Yes", ifelse(
      keep_limits %in% "(1) Usually kept to the limits set", "No", ifelse(
        !is.na(set_limits), "No", NA))) %>%
      factor(levels=c("Yes", "No")))


# Dependence criteria: needed more or less effect
# if either condition has been answered yes, I'll call it a yes
data <- data %>% 
  mutate(need_more_OR_less_effect = ifelse(
    need_more %in%  "(1) Yes" | less_effect %in%  "(1) Yes", "Yes", ifelse(
      need_more %in% "(2) No" | less_effect %in% "(2) No", "No", NA)) %>%
      factor(levels=c("Yes", "No")))


# Dependence criteria: spent time getting/using OR getting over.
# if either condition has been answered yes, I'll call it a yes
data <- data %>% 
  mutate(spent_time_getting_OR_getting_over = ifelse(
    spent_time_getting_using %in% "(1) Yes" | spent_time_over %in% "(1) Yes", "Yes", 
    ifelse(spent_time_getting_using %in% "(2) No" | spent_time_over %in% "(2) No", "No", 
           NA)) %>%
      factor(levels=c("Yes", "No")))


# Dependence criteria: continued use despite mental OR physical problems
data <- data %>% 
  mutate(mental_OR_physical_problems_AND_use = ifelse(
    mental_problems_AND_use %in% "Yes" | physical_problems_AND_use %in% "Yes", 
    "Yes", ifelse(
      mental_problems_AND_use %in% levels(mental_problems_AND_use[2]) | physical_problems_AND_use %in% levels(physical_problems_AND_use[2]), 
      "No", NA)) %>%
      factor(levels=c("Yes", "No")))

# Dependence criteria: Wanted and able to cut down
data <- data %>% 
  mutate(unable_cut = ifelse(
    want_cut_down %in% "(2) No", "No", # Never wanted to cut
    ifelse(able_cut_down %in% "(2) No", "Yes",
           ifelse(able_cut_down %in% "(1) Yes", "No", NA))) %>% # tried; unable to cut.
      factor(levels=c("Yes", "No")))

# Dependence criteria: Stop important activities
data <- data %>% 
  mutate(stop_important_activities = ifelse(
    stop_important_activities %in% "(2) No", "No", # Never wanted to cut
    ifelse(stop_important_activities %in% "(1) Yes", "Yes", NA)) %>% # tried; unable to cut.
      factor(levels=c("Yes", "No")))

# Add counts of symptoms
data <- data %>% mutate(
  symptoms_dep = (spent_time_getting_OR_getting_over=="Yes") + (kept_limits_AND_failed== "Yes") +
    (need_more_OR_less_effect=="Yes") + (unable_cut=="Yes") + (mental_OR_physical_problems_AND_use == "Yes") +
    (stop_important_activities == "(1) Yes"),
  symptoms_abuse = (serious_problems == "(1) Yes" ) + ( dangerous_activities == "(1) Yes" ) + 
    (legal_trouble == "(1) Yes" ) + (family_problems_AND_use =="Yes"))

saveRDS(data, "data/NSDUH_for_drug_policy_allyears.rds")