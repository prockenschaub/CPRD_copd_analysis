###########################################################################
# Author:   Patrick Rockenschaub
# Project:  Preserve Antibiotics through Safe Stewardship (PASS)
#           Primary Care work package 1
#           COPD analysis
#
# File:     attrition.R
# Date:     18/03/2019
# Task:     Calculate the numbers needed for an attrition diagram
#
###########################################################################




# Need to run 03a_derive_tables.R before ----------------------------------

require_tbls(char(practices, patients, cohort))


# Only practices that provide data for the entire period
prac_main <- practices[uts <= main_start & lcd >= study_end, "pracid"]

# Get all patients registered on January 1st 2015
registered <- 
  patients[prac_main, on = "pracid", nomatch = 0] %>% 
         .[enter_date <= study_end & leave_date >= main_start] # study end because entry date set after 1 year continuous registration
nrow(registered)                       



# Follow-up last ----------------------------------------------------------

# Look at who is excluded by age
nrow(registered[(year(main_start) - year(birth_date)) < 35])
nrow(registered[(year(main_start) - year(birth_date)) > 110])
age <- registered[(year(main_start) - year(birth_date)) %between% list(35,110)]                    
                       
# Look at who has (not) got COPD
nrow(age[!cohort, on = "patid"])
copd <- age[(cohort[, "patid"]), on = "patid", nomatch = 0]

# Look at who has (not) got smoking history
smoke <- copd[smoke_at_base, on = "patid", smoke := smoke]
nrow(smoke[smoke == "non"])

smoke %<>% .[smoke != "non"]

# Look at who has (not) got enough follow up or baseline
nrow(smoke[enter_date > ymd("2015-01-01")])
nrow(smoke[leave_date < ymd("2015-12-31")])
nrow(smoke[enter_date <= ymd("2015-01-01") & leave_date >= ymd("2015-12-31")])





# COPD last ---------------------------------------------------------------

# Look at who is excluded by age
nrow(registered[(year(main_start) - year(birth_date)) < 35])
nrow(registered[(year(main_start) - year(birth_date)) > 110])
age_excl <- registered[!(year(main_start) - year(birth_date)) %between% list(35,110)]                    

# Look at who has (not) got enough follow up or baseline
eligible <- registered[enter_date <= ymd("2015-01-01") & leave_date >= ymd("2015-12-31") & 
                         (year(main_start) - year(birth_date)) %between% list(35,110)]
nrow(eligible)


# Look at who has (not) got COPD or smoking history
nrow(eligible[is.na(smoke) | smoke == "non"])
nrow(eligible[!cohort, on = "patid"])

