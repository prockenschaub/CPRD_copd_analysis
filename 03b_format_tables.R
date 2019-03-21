###########################################################################
# Author:   Patrick Rockenschaub
# Project:  Preserve Antibiotics through Safe Stewardship (PASS)
#           Primary Care work package 1
#           COPD analysis
#
# File:     03b_format_tables.R
# Date:     15/03/2019
# Task:     Bring the created tables into a uniform format (factors, etc.)
#
###########################################################################



# Format baseline ---------------------------------------------------------

baseline[, female := factor(female, 0:1, c("male", "female"))]

age_lvls <- c("35-50" = 50, "50-60" = 60, "60-70" = 70, "70-80" = 80, ">80" = Inf)
baseline[, age_cat:= cut(age, c(0, age_lvls), names(age_lvls), right = FALSE)]
baseline[, age_scaled := scale(age)]

baseline[, imd := factor(imd, 1:5)]

lgl <- char(asthma, chd, ckd, dm, hf, pad, stroke, vacc)
fill_na(baseline, lgl, FALSE)
baseline[, (lgl) := map(.SD, factor, c(FALSE, TRUE), c("no", "yes")), .SDcols = lgl]

fill_na(baseline, "obese", "non")
baseline[, obese := factor(obese, c("non", "obese", "severely obese"))]
baseline[, obese := fct_recode(obese, obese = "severely obese")]

baseline[, smoke := factor(smoke, levels = c("ex", "smoke"))]

fev_lvls <- c("very severe" = 30, "severe" = 50, "moderate" = 80, "mild" = Inf)
baseline[, fev := cut(fev, c(-Inf, fev_lvls), names(fev_lvls))]
baseline[, fev := factor(fev, levels = rev(sort(unique(fev))))]

baseline[, mrc := factor(mrc, c(1, 2, 3, 4, 5))]

ae_lvls <- c("0" = 0, "1" = 1, "2" = 2, "3+" = Inf)
baseline[, aecopd := cut(num_aecopd, c(-Inf, ae_lvls), names(ae_lvls))]
baseline[, aecopd := fct_expand(aecopd, "1+ severe")]
baseline[num_sev_ae > 0, aecopd := "1+ severe"]

baseline[, fu_ae_cat := cut(fu_aecopd, c(-Inf, ae_lvls), names(ae_lvls))]
baseline[, fu_ae_cat := fct_expand(fu_ae_cat, "1+ severe")]
baseline[fu_sev_ae > 0, fu_ae_cat := "1+ severe"]

# Create 'centered' for best power
baseline[, age_cat_ctr := fct_relevel(age_cat, "60-70")]
baseline[, imd_ctr := fct_relevel(imd, "3")]
baseline[, fev_ctr := fct_relevel(fev, "moderate")]
baseline[, mrc_ctr := fct_relevel(mrc, "2")]





# Format antibiotics ------------------------------------------------------

# Order the type of prescription
dur_lvls <- c("first", "second", "short-term", "long-term")
abx_cohort[, as := factor(as, dur_lvls)]

# Define the antibiotic class
class_map <- dolk_abx_2018()[, .(prodcode, substance, class, class_desc, subclass, subclass_desc)]
class_map[class == "J01F", class_desc := "Macrolides"]

# Classify based on www.nice.org.uk/guidance/ng114/chapter/Recommendations
first_choice <- c("Amoxicillin", "Doxycycline", "Clarithromycin")
alt_choice <- c("Co-amoxiclav", "Levofloxacin", "Co-trimoxazole")
other_interest <- c("Azithromycin", "Nitrofurantoin", "Flucloxacillin") # For prophylaxis, UTI and SSTI

interest <- c(first_choice, alt_choice, other_interest)

class_map[substance %in% first_choice, class_desc := "first line"]
class_map[substance %in% alt_choice, class_desc := "second line"]
class_map[substance %in% other_interest, class_desc := str_to_lower(substance)]
class_map[!substance %in% interest, class_desc := "other"]

class_map[, c("substance", "subclass", "subclass_desc") := NULL]

# Add the antibiotic class to the prescriptions
abx_class <- 
  abx_cohort[, .(patid, prescdate, prodcode)] %>% 
  .[class_map, on = "prodcode", nomatch = 0]




# Add antibiotics to baseline ---------------------------------------------

# Add summaries of antibiotics during follow-up to baseline
sum_abx <- abx_cohort[, .(num_abx = .N, num_ddd = sum(total_ddd)), by = patid]

baseline[sum_abx, on = "patid", c("num_abx", "num_ddd") := .(num_abx, num_ddd)]
baseline[is.na(num_abx), num_abx := 0]
baseline[is.na(num_ddd), num_ddd := 0]



# Subtract days at risk from baseline -------------------------------------

baseline[util, on = "patid", risk := risk - day_hosp]
baseline %<>% .[risk > 0]


