# load new dataset

# This will load previous NSDUH years, narrow them to variables of interest, and combine them into a single year.

library(tidyverse)
library(stringr)

# Choose variables --------------------------------------------------------
# All the Marijuana ABOD questions
# Demographics: gender, age, education, race
# MJ use: irmjfm

# Choose time frame -------------------------------------------------------
# 2002-2004
# 2013-2015


# Load Time Periods -------------------------------------------------------

# Declare helper function
# Make a function that will use 2014 levels to code the 2015 data.
apply_old_levels_to_integer_data <- function(old_df, new_df, i) {
  # Check if the old column is numeric: if so, then just return back the new_df data.
  if(is.numeric(old_df[[i]]) | is.character(old_df[[i]]) ) return(new_df[[i]])
  # otherwise, try to code the data
  num_uniques <- length(unique(new_df[[i]]))
  coded_levels <- levels(old_df[[i]])
  data_to_code <- new_df[[i]]
  plyr::mapvalues(x = data_to_code, 
                  from = sort(unique(data_to_code)), 
                  to = c(coded_levels, rep(NA, num_uniques - length(coded_levels)))) %>% factor
}

# Remove parens from variable names.
remove_parens <- function(x) {
  str_replace_all(x, pattern = "\\([0-9]\\) +", replacement = '') %>%
    str_replace_all(pattern=" \\(.*\\) ?+", replacement='')
  
}
# Get on with the loading


###### POST
load("data/Data 2015.rda")
nsduh15 <- PUF2015_102016; rm(PUF2015_102016)
colnames(nsduh15) <- colnames(nsduh15) %>% tolower
s15 <- nsduh15 %>% 
  select(mrjlottm:mrjfmctd, irmjrc, mjday30a, abodmrj, abusemrj, depndmrj, irsex, catag3, eduhighcat, newrace2, analwt_c, vestr, verep) %>%
  mutate(year=2015) %>%
  rename(educcat2 = eduhighcat) # to match earlier years.

load("data/Data 2014.rda")
nsduh14 <- da36361.0001; rm(da36361.0001)
colnames(nsduh14) <- colnames(nsduh14) %>% tolower
s14 <- nsduh14 %>% 
  select(mrjlottm:mrjfmctd, irmjrc, mjday30a, abodmrj, abusemrj, depndmrj, irsex, catag3, educcat2, newrace2, analwt_c, vestr, verep) %>%
  mutate(year=2014)

load("data/Data 2013.rda")
nsduh13 <- da35509.0001; rm(da35509.0001)
colnames(nsduh13) <- colnames(nsduh13) %>% tolower
s13 <- nsduh13 %>% 
  select(mrjlottm:mrjfmctd, irmjrc, mjday30a,  abodmrj, abusemrj, depndmrj, irsex, catag3, educcat2, newrace2, analwt_c, vestr, verep) %>%
  mutate(year=2013)

# Apply 2014 levels to the 2015 data (which is not yet factored)
# Will have to factor unfactored variables, and fix mjday30a
for(i in 1:ncol(s15)) s15[[i]] <- apply_old_levels_to_integer_data(s14, s15, i) # fix factor columns/
s15$mjday30a <- ifelse(s15$mjday30a > 30, NA, s15$mjday30a)  # set mjday30a to NA if >30


# Merge
post <- bind_rows(s13, s14, s15)
rm(s13); rm(s14); rm(s15); rm(nsduh13); rm(nsduh14); rm(nsduh15)

# Repeat for t1.

load("data/Data 2002.rda")
temp <- da03903.0001; rm(da03903.0001)
colnames(temp) <- colnames(temp) %>% tolower
s02 <- temp %>%
  select(mrjlottm:mrjfmctd, irmjrc, mjday30a, abodmrj, abusemrj, depndmrj, irsex, catag3, educcat2, newrace2, analwt_c, vestr, verep) %>%
  mutate(year=2002)

load("data/Data 2003.rda")
temp <- da04138.0001; rm(da04138.0001)
colnames(temp) <- colnames(temp) %>% tolower
s03 <- temp %>%
  select(mrjlottm:mrjfmctd, irmjrc, mjday30a, abodmrj, abusemrj, depndmrj,  irsex, catag3, educcat2, newrace2, analwt_c, vestr, verep) %>%
  mutate(year=2003)

load("data/Data 2004.rda")
temp <- da04373.0001; rm(da04373.0001)
colnames(temp) <- colnames(temp) %>% tolower
s04 <- temp %>%
  select(mrjlottm:mrjfmctd, irmjrc, mjday30a, abodmrj, abusemrj, depndmrj, irsex, catag3, educcat2, newrace2, analwt_c, vestr, verep) %>%
  mutate(year=2004)

pre <- bind_rows(s02, s03, s04)
rm(s02); rm(s03); rm(s04); rm(temp)


# Other Years -------------------------------------------------------------

load("data/Data 2005.rda")
temp <- da04596.0001; rm(da04596.0001)
colnames(temp) <- colnames(temp) %>% tolower
s05 <- temp %>%
  select(mrjlottm:mrjfmctd, irmjrc, mjday30a, abodmrj, abusemrj, depndmrj, irsex, catag3, educcat2, newrace2, analwt_c, vestr, verep) %>%
  mutate(year=2005)

load("data/Data 2006.rda")
temp <- da21240.0001; rm(da21240.0001)
colnames(temp) <- colnames(temp) %>% tolower
s06 <- temp %>%
  select(mrjlottm:mrjfmctd, irmjrc, mjday30a, abodmrj, abusemrj, depndmrj,  irsex, catag3, educcat2, newrace2, analwt_c, vestr, verep) %>%
  mutate(year=2006)

load("data/Data 2007.rda")
temp <- da23782.0001; rm(da23782.0001)
colnames(temp) <- colnames(temp) %>% tolower
s07 <- temp %>%
  select(mrjlottm:mrjfmctd, irmjrc, mjday30a, abodmrj, abusemrj, depndmrj, irsex, catag3, educcat2, newrace2, analwt_c, vestr, verep) %>%
  mutate(year=2007)

load("data/Data 2008.rda")
temp <- da26701.0001; rm(da26701.0001)
colnames(temp) <- colnames(temp) %>% tolower
s08 <- temp %>%
  select(mrjlottm:mrjfmctd, irmjrc, mjday30a, abodmrj, abusemrj, depndmrj, irsex, catag3, educcat2, newrace2, analwt_c, vestr, verep) %>%
  mutate(year=2008, mrjlottm = factor(mrjlottm, levels=1:2, labels=c("(1) Yes", "(2) No")))

load("data/Data 2009.rda")
temp <- da29621.0001; rm(da29621.0001)
colnames(temp) <- colnames(temp) %>% tolower
s09 <- temp %>%
  select(mrjlottm:mrjfmctd, irmjrc, mjday30a, abodmrj, abusemrj, depndmrj, irsex, catag3, educcat2, newrace2, analwt_c, vestr, verep) %>%
  mutate(year=2009, mrjlottm = factor(mrjlottm, levels=1:2, labels=c("(1) Yes", "(2) No")))

load("data/Data 2010.rda")
temp <- da32722.0001; rm(da32722.0001)
colnames(temp) <- colnames(temp) %>% tolower
s10 <- temp %>%
  select(mrjlottm:mrjfmctd, irmjrc, mjday30a, abodmrj, abusemrj, depndmrj, irsex, catag3, educcat2, newrace2, analwt_c, vestr, verep) %>%
  mutate(year=2010, mrjlottm = factor(mrjlottm, levels=1:2, labels=c("(1) Yes", "(2) No")))

load("data/Data 2011.rda")
temp <- da34481.0001; rm(da34481.0001)
colnames(temp) <- colnames(temp) %>% tolower
s11 <- temp %>%
  select(mrjlottm:mrjfmctd, irmjrc, mjday30a, abodmrj, abusemrj, depndmrj, irsex, catag3, educcat2, newrace2, analwt_c, vestr, verep) %>%
  mutate(year=2011)

load("data/Data 2012.rda")
temp <- da34933.0001; rm(da34933.0001)
colnames(temp) <- colnames(temp) %>% tolower
s12 <- temp %>%
  select(mrjlottm:mrjfmctd, irmjrc, mjday30a, abodmrj, abusemrj, depndmrj, irsex, catag3, educcat2, newrace2, analwt_c, vestr, verep) %>%
  mutate(year=2012)


s08 <- purrr::pmap(list(numeric = s08, factored=s11),
           function(numeric, factored)
             if(class(factored) == "factor") {
             factor(numeric, levels=sort(unique(numeric)), labels = levels(factored))
             } else {
               numeric
             }
           ) %>% as.data.frame()

s07 <- purrr::pmap(list(numeric = s07, factored=s11),
                   function(numeric, factored)
                     if(class(factored) == "factor") {
                       factor(numeric, levels=sort(unique(numeric)), labels = levels(factored))
                     } else {
                       numeric
                     }
                   ) %>% as.data.frame()

s09 <- purrr::pmap(list(numeric = s09, factored=s11),
                   function(numeric, factored)
                     if(class(factored) == "factor") {
                       factor(numeric, levels=sort(unique(numeric)), labels = levels(factored))
                     } else {
                       numeric
                     }
                   ) %>% as.data.frame()

s10 <- purrr::pmap(list(numeric = s10, factored=s11),
                   function(numeric, factored)
                     if(class(factored) == "factor") {
                       factor(numeric, levels=sort(unique(numeric)), labels = levels(factored))
                     } else {
                       numeric
                     }
                   ) %>% as.data.frame()



middle <- bind_rows(s05, s06, s07, s08, s09, s10, s11, s12)
rm(s05); rm(s06); rm(s07); rm(temp); rm(s08); rm(s09); rm(s10); rm(s11); rm(s12)



# combine to single df ----------------------------------------------------
pre$t <- 1
post$t <- 2
middle$t <- NA
prepost <- bind_rows(list(pre, post))
all <- bind_rows(list(pre, post, middle))




# Prep / Fixing -----------------------------------------------------------

# fix bug introduced by the race variable.
all$educcat2 <- remove_parens(all$educcat2)

# fix bug for abuse/dependence
all$abodmrj <- remove_parens(all$abodmrj)
all$abodmrj[all$abodmrj=="Yes "] <- "Yes"

all$depndmrj <- remove_parens(all$depndmrj)
all$abusemrj <- remove_parens(all$abusemrj)


# prepost <- readRDS("data/NSDUH_for_drug_policy_raw.rds")
# Save --------------------------------------------------------------------
# saveRDS(all %>% filter(year%in%c(2002:2004, 2012:2014)), "data/NSDUH_for_drug_policy_raw.rds")
saveRDS(all, "data/NSDUH_for_drug_policy_raw.rds")
rm(pre); rm(post)


