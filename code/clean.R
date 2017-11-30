# Cleaning data.
library(tidyverse)

readRDS("data/NSDUH_for_drug_policy_raw.rds") %>%
  mutate(plyr::mapvalues(x = t, from=c(1,2), to=c("2002-2004", "2013-2015")),
         depndmrj = factor(depndmrj)) %>% 
  
    dplyr::transmute(
    
    ##### Variables related to survey structure
    wt=analwt_c,
    verep=verep,
    vestr=vestr,
    year=year,
    t=t,
    
    ##### Demographic and Use variables
    newrace2=newrace2,
    catag3=catag3,
    educcat2=educcat2,
    irsex=irsex,
    use_recency = ifelse(
      irmjrc %in% levels(irmjrc)[4], "Never", ifelse(
        irmjrc %in% levels(irmjrc)[3], "Lifetime", ifelse(
          irmjrc %in% levels(irmjrc)[2], "PY", ifelse(
            irmjrc %in% levels(irmjrc)[1] & mjday30a <= 20, "PM", ifelse(
              mjday30a > 20, "DND", NA))))),
    dnd = use_recency == "DND",
    
    ##### Dependence variables
    depndmrj = depndmrj,
    
    ##### Symptoms (note different rules for each)
    ### Simple:
    stop_important_activities = 
      factor(mrjlsact, levels=c("(1) Yes", "(2) No"), labels=c("Yes", "No")),
    ### Any one of two:
    # Lots of time spent getting, using, or getting over
    spent_time_getting_OR_getting_over = 
      ifelse(mrjlottm %in% "(1) Yes" | mrjgtovr %in% "(1) Yes", "Yes", 
             ifelse(mrjlottm %in% "(2) No" | mrjgtovr %in% "(2) No", "No", NA)) %>%
      factor(levels=c("Yes", "No")),
    # Needing more or getting less effect
    need_more_OR_less_effect = 
      ifelse(mrjndmor %in%  "(1) Yes" | mrjlsefx %in%  "(1) Yes", "Yes",
             ifelse(mrjndmor %in% "(2) No" | mrjlsefx %in% "(2) No", "No", NA)) %>%
      factor(levels=c("Yes", "No")),
    ### Conditional (generates NAs if first condition not met)
    set_limits_BUT_failed = 
      # Numerator: Said they exceeded limits
      ifelse(mrjkplmt %in% "(2) Often used more than intended", "Yes",
             # Denominator: Answered Q about whether they tried limits
             ifelse(mrjlimit %in% c("(1) Yes", "(2) No"), "No", NA)) %>%
      factor(levels=c("Yes", "No")),
    set_limits_BUT_failed_NAs = factor(mrjkplmt, levels=levels(mrjkplmt), labels=c("No", "Yes")),
    # No if "Usually kept to limits set"
    # Yes if "Often used more than intended"
    unable_cut_down = 
      # Numerator: said they were not able to make an attempt to cut down.
      ifelse(mrjcutev %in% "(2) No", "Yes",
             # Denominator: answered Q about whether they tried to cut
             ifelse(mrjcutdn %in% c("(1) Yes", "(2) No"), "No", NA)) %>%
      factor(levels=c("Yes", "No")),
    unable_cut_down_NAS = factor(mrjcutev, levels=levels(mrjcutev), labels=c("No", "Yes")),
    # No if "able to cut down or stop using marijuana every time you wanted to or tried to"
    # Yes if "unable..."
    use_despite_problems = 
      # Numerator: said they continued use despite a mental/emo or phys problem
      ifelse(mrjemctd %in% "(1) Yes" | mrjphctd %in% "(1) Yes", "Yes",
             # Denominator: answered either Q about whether they had problems
             ifelse(mrjemopb %in% c("(1) Yes", "(2) No") | mrjphlpb  %in% c("(1) Yes", "(2) No"), "No", NA)) %>%
      factor(levels=c("Yes", "No")),
    use_despite_problems_NAs = 
      # this is a conditional based on TWO questions.
      ifelse(mrjemctd %in% "(1) Yes" | mrjphctd %in% "(1) Yes", "Yes",
             ifelse(mrjemctd %in% "(2) No" | mrjphctd %in% "(2) No", "No", NA)) %>%
      factor(levels=c("Yes", "No"))) %>% 
# Yes if reported to continue use despite mental/emotional or physical problems
# No if reported to stop use in face of either problem.
# NA if did not report either problem
<<<<<<< HEAD
  
  # Save this to a RDS file
  saveRDS("data/NSDUH_for_drug_policy_clean.rds")

=======

  mutate(
    symptoms_dep = (spent_time_getting_OR_getting_over=="Yes") + (kept_limits_AND_failed== "Yes") +
    (need_more_OR_less_effect=="Yes") + (unable_cut=="Yes") + (mental_OR_physical_problems_AND_use == "Yes") +
    (stop_important_activities == "(1) Yes")) %>%
  
  # Save this to a RDS file
  saveRDS("data/NSDUH_for_drug_policy_clean.rds")
>>>>>>> a64f5c96b11980b49017181363c9bd300af397b4

getwd()
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



