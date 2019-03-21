###########################################################################
# Author:   Patrick Rockenschaub
# Project:  Preserve Antibiotics through Safe Stewardship (PASS)
#           Primary Care work package 1
#           COPD analysis
#
# File:     03a_derive_tables.R
# Date:     07/11/2018
# Task:     Use the extracted data to create tables for analysis
#
###########################################################################


library(forcats)


# Create a table of baseline values ---------------------------------------
## @knitr baseline
#+ baseline, include = FALSE

require_tbls(char(practices, cohort, comorb, obese, smoke, 
                  aecopd, severe_aecopd, fev, mrc, flu_vacc))


# Time ranges used to detect and remove outliers
y5 <- list(main_start %m-% years(5), main_start %m-% days(1))
m18 <- list(main_start %m-% months(18), main_start %m-% days(1))
m12 <- list(main_start %m-% months(12), main_start %m-% days(1))

# Practices with complete year follow-up
prac_main <- practices[uts <= main_start & lcd >= study_end, "pracid"]

# Age, sex, IMD
baseline <- 
  cohort[prac_main, on = "pracid", nomatch = 0] %>% 
       .[enter_date <= main_start & leave_date >= study_end,  
         .(patid, pracid, 
           female, 
           age = year(main_start) - year(birth_date), 
           imd,
           leave_date, 
           risk = time_length(main_start %--% study_end, u = "days") + 1L)]

baseline %<>% .[age %between% list(35, 110)] # Only a handful are younger


# Comorbidities, obesity, smoking
comorb_at_base <- 
  comorb[, c(.(patid = patid), map(.SD, ~ . < main_start)), 
         .SDcols = asthma:stroke]

setorder(obese, patid, eventdate)
obese_at_base <- closest_in(obese[, .(patid, eventdate, obese)], y5)


setorder(smoke, patid, eventdate)
smoke_at_base <- closest_in(smoke[, .(patid, eventdate, smoke = status)], y5)

ever_smoked <- smoke[eventdate < study_start & status != "non", .(patid)]
smoke_at_base[ever_smoked, on = "patid", smoke := if_else(smoke == "non", "ex", smoke)]

# Baseline number of exacerbations
aecopd_at_base <- aecopd[eventdate %between% m12]
aecopd_at_base %<>% .[, .(num_aecopd = .N), by = patid]

aecopd_fu <- aecopd[eventdate %between% list(main_start, study_end)]
aecopd_fu %<>% .[, .(fu_aecopd = .N), by = patid]

sev_ae_at_base <- severe_aecopd[admidate %between% m12 | 
                                discharged %between% m12]
sev_ae_at_base %<>% .[, .(num_sev_ae = .N), by = patid]

sev_ae_fu <- severe_aecopd[admidate %between% list(main_start, study_end) | 
                           discharged %between% list(main_start, study_end)]
sev_ae_fu %<>% .[, .(fu_sev_ae = .N), by = patid]

# COPD severity
fev_at_base <- closest_in(fev[, !("method")], m12)
mrc_at_base <- closest_in(mrc[, !("medcode")], m12)

# Immunisation
vacc_at_base <- closest_in(flu_vacc[, !("medcode")], m12)
vacc_at_base[, vacc := TRUE]


# Combine to baseline table
baseline <-   
  mget(c("baseline", ls()[ls() %like% "at_base"])) %>% 
  reduce(merge, by = "patid", all.x = TRUE)

baseline[is.na(num_aecopd), num_aecopd := 0]
baseline[is.na(num_sev_ae), num_sev_ae := 0]


baseline[aecopd_fu, on = "patid", fu_aecopd := fu_aecopd]
baseline[is.na(fu_aecopd), fu_aecopd := 0]
baseline[sev_ae_fu, on = "patid", fu_sev_ae := fu_sev_ae]
baseline[is.na(fu_sev_ae), fu_sev_ae := 0]

# Limit to patients with a record of ever having smoked
baseline %<>% .[smoke %in% c("smoke", "ex")]



# Get all the antibiotics within the study period -------------------------
## @knitr abx_cohort
#+ abx_cohort, include = FALSE

require_tbls(char(cohort, baseline, abx, reason, presc_as))


# Limit the antibiotics to all patients in the cohort and within 
# the main study period
abx_cohort <- 
  abx[(baseline[, .(patid)]), on = "patid", nomatch = 0] %>% 
    .[(cohort[, .(patid, leave_date)]), 
      on = "patid", nomatch = 0] %>% 
    .[reason, on = "abx_id", 
      c("system", "conditions") := .(system, conditions)] %>% 
    .[presc_as, on = "abx_id", as := as] %>% 
    .[prescdate %between% list(main_start, study_end)] %>% 
    .[, .(patid, abx_id, prodcode, prescdate, substance, 
          strength, qty, ndd, system, conditions, as, total_ddd)]

# Collapse systems and conditions
abx_cohort[, system := factor(system)]
abx_cohort[, system := fct_lump(system, 5L, other = "other")]


main_systems <- c("rt/ent", "urogenital tract", "skin and wounds")
abx_cohort[!(system %in% main_systems), 
           conditions := list(list(NA_character_))]


# Define the order of precedence of conditions to get a unique 
# condition for each within each body system
resp_class <-
  read_excel(file.path(subfolder, "resp_indication_chapters.xlsx"), 
             col_names = c("code", "type", "desc", "condition", "hier"),
             skip = 1)
setDT(resp_class)
setorder(resp_class, type, hier)


c_order <- c(unique(resp_class$condition),
             "genital tract", "urinary tract", "unspecific urogenital",
             "cellulitis", "boil, cyst, abscess", "bites", "wounds", 
                "ingrown/infected nail", "acne", "other skin", 
                "unspecific")

choose_cond <- function(conditions){
  # Among the list passed into the function, choose the value that is 
  # earliest in the vecotr c_order given above
  #
  # Args: 
  #   conditions - a character vector with all conditions that apply 
  #                to one antibiotic
  #
  # Result:
  #   a character vector of length 1
  
  if(all(is.na(conditions))) {
    return(NA_character_)
  }
  
  pos <- map_int(unique(conditions), 
                 ~ which(str_detect(c_order, str_c("^", ., "$"))))
  conditions[which.min(pos)]
}

abx_cohort[!is.na(conditions), 
           condition := map_chr(conditions, choose_cond)]





# Calculate the health resource utilisation -------------------------------
## @knitr util
#+ util, include = FALSE

require_tbls(char(cohort, baseline, abx, cons, admission, hosp_diag, ae))


util <- cohort[, .(patid)] %>% 
             .[(baseline[, "patid"]), on = "patid"]

# Consultations in primary care
cons_obs <-
  cons[util, on = "patid", nomatch = 0] %>% 
     .[, .(patid,
           in_obs = eventdate %between% list(main_start, 
                                             study_end),
           in_prev= eventdate %between% list(main_start %m-% months(6), 
                                             main_start %m-% days(6)))] %>% 
     .[, .(num_cons = sum(in_obs), num_cons_6m = sum(in_prev)), by = patid]


# Hospital admissions
admission[(hosp_diag[icd %like% "^J"]), on = "spno", resp_icd := TRUE]
admission[is.na(resp_icd), resp_icd := FALSE]


time_within <- function(activity, in_period){
  # Calculate the number of days of activity that fall within another 
  # time interval. Overlap between intervals is compared pair-wise
  #
  # Args:
  #   activity - vector of activity intervals (e.g. hospital stay)
  #   in_period - vector of corresponding time windows
  #
  # Result:
  #   An integer vector with the overlap in days 
  #
  # NOTE: The first day (i.e. start) of an activity is not counted
  
  start <- pmax(int_start(activity), int_start(in_period))
  end <- pmin(int_end(activity), int_end(in_period))
  
  len <- time_length(start %--% end, unit = "days")
  len[len < 0] <- NA
  len + (int_start(activity) < int_start(in_period))
}


hosp_stays <- 
  admission[util, on = "patid", nomatch = 0] %>% 
          .[, `:=`(in_obs = time_within(admidate %--% discharged,
                                        main_start %--% study_end),
                   in_prev= time_within(admidate %--% discharged,
                                        (main_start %m-% months(6)) %--% 
                                        (main_start %m-% days(1))))]


hosp_obs <- 
  hosp_stays[, .(num_hosp = sum(!is.na(in_obs)), 
                 num_hosp_resp = sum(resp_icd & !is.na(in_obs)), 
                 day_hosp = sum(in_obs, na.rm = TRUE), 
                 num_hosp_6m = sum(!is.na(in_prev)), 
                 num_hosp_resp_6m = sum(resp_icd & !is.na(in_prev)), 
                 day_hosp_6m = sum(in_prev, na.rm = TRUE)),
             by = patid]

remove(hosp_stays, time_within)



# Visits to the A&E department
ae_obs <-
  ae[util, on = "patid", nomatch = 0] %>% 
   .[, .(patid,
         in_obs = arrivaldate %between% list(main_start, 
                                             study_end),
         in_prev= arrivaldate %between% list(main_start %m-% months(6), 
                                             main_start %m-% days(1)))] %>% 
   .[, .(num_ae = sum(in_obs), num_ae_6m = sum(in_prev)), by = patid]


# Death during the observation period
death_obs <- 
  cohort[(baseline[, .(patid)]), on = "patid",
         .(patid, 
           death = if_else(death_date %between% .(main_start, study_end), 
                           "yes", "no", "no"))]


# Add together all the measures
util %<>% 
  list(cons_obs, hosp_obs, ae_obs, death_obs) %>% 
  reduce(merge, by = "patid", all.x = TRUE)

util_num <- util[, which(map_lgl(.SD, is.numeric))]
fill_na(util, names(util_num), 0)









# Create a table of baseline values for all patients ----------------------
## @knitr everyone
#+ everyone, include = FALSE

require_tbls(char(practices, patients, cohort, comorb, obese, smoke, 
                  fev, mrc, flu_vacc))

# Update the entry date for the COPD patients
patients[cohort, on = "patid", enter_date := i.enter_date]

# Time ranges used to detect and remove outliers
y5 <- list(main_start %m-% years(5), main_start %m-% days(1))
m18 <- list(main_start %m-% months(18), main_start %m-% days(1))
m12 <- list(main_start %m-% months(12), main_start %m-% days(1))

# Practices with complete year follow-up
prac_main <- practices[uts <= main_start & lcd >= study_end, "pracid"]

# Age, sex, IMD
everyone <- 
  patients[prac_main, on = "pracid", nomatch = 0] %>% 
         .[enter_date <= main_start & leave_date >= study_end,  
            .(patid, pracid, leave_date,
              female, 
              age = year(main_start) - year(birth_date), 
              imd,
              risk = time_length(main_start %--% study_end, u = "days") + 1L)]

everyone %<>% .[age %between% list(35, 110)]


# Comorbidities, obesity, smoking
comorb_all <- 
  comorb[, c(.(patid = patid), map(.SD, ~ . < main_start)), 
         .SDcols = asthma:stroke]

setorder(obese, patid, eventdate)
obese_all <- closest_in(obese[, .(patid, eventdate, obese)], y5)


setorder(smoke, patid, eventdate)
smoke_all <- closest_in(smoke[, .(patid, eventdate, smoke = status)], y5)

ever_smoked <- smoke[eventdate < study_start & status != "non", .(patid)]
smoke_all[ever_smoked, on = "patid", smoke := if_else(smoke == "non", "ex", smoke)]

# Combine to baseline table
everyone <-   
  mget(c("everyone", ls()[ls() %like% "_all$"])) %>% 
  reduce(merge, by = "patid", all.x = TRUE)




require_tbls(char(cohort, baseline, abx, cons, admission, hosp_diag, ae))


util_eone <- everyone[, .(patid)]

# Consultations in primary care
cons_obs <-
  cons[util_eone, on = "patid", nomatch = 0] %>% 
  .[, .(patid,
        in_obs = eventdate %between% list(main_start, 
                                          study_end),
        in_prev= eventdate %between% list(main_start %m-% months(6), 
                                          main_start %m-% days(6)))] %>% 
  .[, .(num_cons = sum(in_obs), num_cons_6m = sum(in_prev)), by = patid]


# Hospital admissions
admission[(hosp_diag[icd %like% "^J"]), on = "spno", resp_icd := TRUE]
admission[is.na(resp_icd), resp_icd := FALSE]


time_within <- function(activity, in_period){
  # Calculate the number of days of activity that fall within another 
  # time interval. Overlap between intervals is compared pair-wise
  #
  # Args:
  #   activity - vector of activity intervals (e.g. hospital stay)
  #   in_period - vector of corresponding time windows
  #
  # Result:
  #   An integer vector with the overlap in days 
  #
  # NOTE: The first day (i.e. start) of an activity is not counted
  
  start <- pmax(int_start(activity), int_start(in_period))
  end <- pmin(int_end(activity), int_end(in_period))
  
  len <- time_length(start %--% end, unit = "days")
  len[len < 0] <- NA
  len + (int_start(activity) < int_start(in_period))
}


hosp_stays <- 
  admission[util_eone, on = "patid", nomatch = 0] %>% 
  .[, `:=`(in_obs = time_within(admidate %--% discharged,
                                main_start %--% study_end),
           in_prev= time_within(admidate %--% discharged,
                                (main_start %m-% months(6)) %--% 
                                  (main_start %m-% days(1))))]


hosp_obs <- 
  hosp_stays[, .(num_hosp = sum(!is.na(in_obs)), 
                 num_hosp_resp = sum(resp_icd & !is.na(in_obs)), 
                 day_hosp = sum(in_obs, na.rm = TRUE), 
                 num_hosp_6m = sum(!is.na(in_prev)), 
                 num_hosp_resp_6m = sum(resp_icd & !is.na(in_prev)), 
                 day_hosp_6m = sum(in_prev, na.rm = TRUE)),
             by = patid]

remove(hosp_stays, time_within)



# Visits to the A&E department
ae_obs <-
  ae[util_eone, on = "patid", nomatch = 0] %>% 
  .[, .(patid,
        in_obs = arrivaldate %between% list(main_start, 
                                            study_end),
        in_prev= arrivaldate %between% list(main_start %m-% months(6), 
                                            main_start %m-% days(1)))] %>% 
  .[, .(num_ae = sum(in_obs), num_ae_6m = sum(in_prev)), by = patid]


# Death during the observation period
death_obs <- 
  cohort[(baseline[, .(patid)]), on = "patid",
         .(patid, 
           death = if_else(death_date %between% .(main_start, study_end), 
                           "yes", "no", "no"))]


# Add together all the measures
util_eone %<>% 
  list(cons_obs, hosp_obs, ae_obs, death_obs) %>% 
  reduce(merge, by = "patid", all.x = TRUE)

util_eone_num <- util_eone[, which(map_lgl(.SD, is.numeric))]
fill_na(util_eone, names(util_eone_num), 0)




everyone[, female := factor(female, 0:1, c("male", "female"))]

age_lvls <- c("35-50" = 50, "50-60" = 60, "60-70" = 70, "70-80" = 80, ">80" = Inf)
everyone[, age_cat:= cut(age, c(0, age_lvls), names(age_lvls), right = FALSE)]
everyone[, age_scaled := scale(age)]

everyone[, imd := factor(imd, 1:5)]

everyone[baseline, on = "patid", copd := TRUE]

lgl <- char(asthma, chd, ckd, copd, dm, hf, pad, stroke)
fill_na(everyone, lgl, FALSE)

everyone[, num_com := .(asthma + chd + ckd + dm + hf + pad + stroke)]
everyone[, num_com := cut(num_com, c(-Inf, 0, 1,  Inf), c("0", "1", "2"))]

everyone[, (lgl) := map(.SD, factor, c(FALSE, TRUE), c("no", "yes")), .SDcols = lgl]

fill_na(everyone, "obese", "non")
everyone[, obese := factor(obese, c("non", "obese", "severely obese"))]
everyone[, obese := fct_recode(obese, obese = "severely obese")]

everyone[, smoke := factor(smoke, levels = c("ex", "smoke", "non"))]



