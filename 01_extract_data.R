###########################################################################
# Author:   Patrick Rockenschaub
# Project:  Preserve Antibiotics through Safe Stewardship (PASS)
#           Primary Care work package 1
#           COPD analysis
#
# File:     01_extract_data.R
# Date:     07/11/2018
# Task:     Extract all necessary records from the database and store them
#           in local R files for reproducability
#
###########################################################################


# Path from project directory to this file
# NOTE: must be set in each programme separately
subfolder <- "04_copd"

# Initialise the workspace
source(file.path(subfolder, "00_init.R"))




# Select all COPD diagnoses in the database -------------------------------
## @knitr copd
#+ copd, include = FALSE

# Select all relevant QOF codes for patient selection
def_copd_codes <- 
  codes_qof_db() %>% 
  filter(comorbidity == "copd")


# Get the COPD patients, keeping only the earliest diagnoses
def_copd <-
  qof_db() %>% 
  records_db(def_copd_codes) %>% 
  filter(!is.na(eventdate)) %>% 
  compute(name = "copd")

copd <- collect_dt(def_copd, convert = TRUE)


# Keep only the earliest event
first_copd <- first_record(copd)
setnames(first_copd, "eventdate", "copd_date")




# Select practices --------------------------------------------------------
## @knitr practices
#+ practices, include = FALSE

# Obtain information on when practices provided data
def_practice <- 
  practice_db() %>% 
  filter(linked == 1L) %>% 
  compute(name = "practices")

practices <- collect_dt(def_practice, convert = TRUE)



# Select patient population -----------------------------------------------
## @knitr patients
#+ patients, include = FALSE

# Define start and end dates
def_patients <-
  study_population_db(link = TRUE) %>% 
  in_date(study_start, study_end) %>% 
  # semi_join(def_copd, by = "patid") %>%  # Select all patients instead
  semi_join(def_practice, by = "pracid") %>% 
  left_join(imd_db(), by = c("patid", "pracid"))

# Apply exclusion criteria
def_patients <-
  def_patients %>% 
  filter(
    !is.na(birth_date), 
    !is.na(female), 
    !is.na(imd)
  ) %>% 
  select(patid, pracid, female, imd, birth_date, 
         death_date, enter_date, leave_date)

def_patients <- compute(def_patients, name = "patients")

patients <- collect_dt(def_patients, convert = TRUE)




# Define COPD cohort ------------------------------------------------------
## @knitr cohort
#+ cohort, include = FALSE

cohort <- merge(patients, first_copd[, .(patid, copd_date)], on = "patid")
cohort[, enter_date := pmax(enter_date, copd_date)]
cohort %<>% .[enter_date <= leave_date]


select_cohort <- function(tbl_sql){
  # This function constraints a table in the database to patients with COPD
  #
  # Args:
  #   tbl_sql - any table reference in the database
  #
  # Result:
  #   the subset of the tabel corresponding to patients with COPD
  
  tbl_sql %>% 
    semi_join(def_copd, by = "patid") %>% 
    semi_join(def_patients, by = "patid")
}




# Select all systemic antibiotic prescribing ------------------------------
## @knitr abx
#+ abx, include = FALSE

# Define the included and excluded BNF chapters
bnf_systemic <- "0501"
bnf_excl <- c("050109", "050110")

# Get all prescription data for the above defined study period  
def_abx <-
  abx_bnf_db(bnf_systemic, bnf_excl) %>% 
  # semi_join(def_copd, by = "patid") %>% # Select all patients instead
  semi_join(def_patients, by = "patid") %>% 
  in_date(study_start %m-% lookback, study_end) %>% 
  select(patid, prescdate = eventdate, prodcode, consid, 
         qty, ndd, issueseq) %>% 
  arrange(patid, eventdate, issueseq)

abx <- collect_dt(def_abx, convert = TRUE)


# Some drugs are classified in 5.1., but are crossover-products
# Exclude those
abx_info <- antibiotics() %>% collect_dt()

atc_a02b <- abx_info[atcchapter == "A02B"]
abx %<>% .[!atc_a02b, on = "prodcode"]
remove(atc_a02b)


abx %<>% .[!(abx_info[group %in% c("Antifungal", "Antileprotic", 
                                   "Antituberculosis", "No antibiotic")]),
             on = "prodcode"]


# No unique identifier of prescription exists in the database, so create 
# one for this project.
#
# NOTE: This identifier might change with each new database query, so the 
#       identifier is only valid within the same session (and not between
#       sessions)
abx <- abx[order(patid, prescdate, prodcode, qty, issueseq)]
abx[, "abx_id" := .I]

# Add the antibiotic name
abx_info <- collect_dt(antibiotics())
abx_info[, abx_substance := str_to_lower(substance)]
abx[abx_info, on = "prodcode", substance := abx_substance]



# Standardise form, route and strength ------------------------------------
## @knitr form_route_strength
#+ form_route_strength, include = FALSE

# Add formulation?route/strength information
pack_info <- collect_dt(product())
abx[pack_info, on = "prodcode", 
    c("form", "route", "strength") := .(formulation, route, strength)]

# Standardise route to WHO DDD abbrevitations in order to map DDDs
# and exclude ocular & dental & topicals, which are not systemic

route <- 
  read_excel(file.path(lu_dir, "PASS", "ROUTE.xlsx")) %>% 
  as.data.table()

non_O_P <- route[str_detect(who, "[^/OP]")]

abx %<>% .[!non_O_P, on = "route"]
abx[route, on = "route", route := who]
abx[, form := NULL]

remove(route, non_O_P)


# Manually add some antibiotics with missing package information. Stop
# programme if one or more strengths are still missing afterwards
abx[prodcode %in% c(391, 4116, 14749, 20869, 26510, 55840, 65446), 
    route := "P"]

abx[prodcode == 64501, c("route", "strength") := .("O", "3")]
abx[prodcode == 64172, c("route", "strength") := .("P", "4")]
abx[prodcode == 57292, c("route", "strength") := .("O", "250mg/5ml")]
abx[prodcode == 52669, c("route", "strength") := .("P", "200mg/5ml")]
abx[prodcode == 53673, c("route", "strength") := .("P", "500mg/100ml")]
abx[prodcode == 55112, c("route", "strength") := .("O", "500mg")]
abx[prodcode == 49349, c("route", "strength") := .("O", "125mg/5ml")]
abx[prodcode == 50239, c("route", "strength") := .("O", "125mg/5ml")]
abx[prodcode == 58525, c("route", "strength") := .("P", "2g")]
abx[prodcode == 59813, c("route", "strength") := .("O", "60mg/5ml")]
abx[prodcode == 65039, c("route", "strength") := .("P", "360mg/120ml")]


# Exclude antibiotics for which no strength is available. Check that none
# of them fall into the main study period
stopifnot(max(abx[is.na(strength)]$prescdate) < ymd("2015-01-01"))

abx %<>% .[!(prodcode %in% c(439, 15355, 24093, 24643))]

stopifnot(all(!is.na(abx$strength)))





# Add DDD information for all antibiotics ---------------------------------
## @knitr ddd
#+ ddd, include = FALSE

# Some drugs are combined with a second substance, like co-amoxiclav
# Make sure those are in the right order

comb_rel <- dtribble(
  ~ substance                , ~ first_geq,
  #--------------------------|------------#
  "ampicillin/flucloxacillin", TRUE       ,
  "co-amoxiclav"             , TRUE       ,
  "co-trimoxazole"           , FALSE      ,
  "pip-taz"                  , FALSE      ,
  "imipenem"                 , TRUE     
)

combinations <- 
  abx[str_detect(strength, "\\+"), .(substance, strength)] %>% 
  unique() %>% 
  .[order(substance, strength)]

stopifnot(nrow(combinations[!comb_rel, on = "substance"]) == 0)

combinations[, first := str_extract(strength, "^[0-9]*")]
combinations[, second := str_extract(strength, "(?<=(\\+|\\+ ))[^ ][0-9]*")]
combinations[, c("first", "second") := 
               map(.SD[, .(first, second)], as.numeric)]

combinations[, rel := .(first >= second)]

switched <- combinations[!comb_rel, on = .(substance, rel = first_geq)]

# Manually review if new ones are added and then automatically correct
# using the following list
correct <- dtribble(
  ~ substance     , ~strength             , ~correct              ,
  #---------------|-----------------------|-----------------------#
  "co-trimoxazole", "80mg/1ml + 16mg/1ml" , "16mg/1ml + 80mg/1ml" ,
  "co-trimoxazole", "400mg + 80mg"        , "80mg + 400mg"        ,
  "co-trimoxazole", "200mg + 40mg/5ml"    , "40mg/5ml + 200mg/1ml",
  "co-trimoxazole", "800mg + 160mg"       , "160mg + 800mg"       ,
  "co-amoxiclav"  , "31mg + 125mg/5ml"    , "125mg/5ml + 31mg/5ml",
  "co-amoxiclav"  , "62mg + 250mg/5ml"    , "250mg/5ml + 62mg/5ml",
  "co-amoxiclav"  , "125mg + 250mg"       , "250mg + 125mg"       ,
  "co-amoxiclav"  , "57mg + 400mg/5ml"    , "400mg/5ml + 57mg/5ml",
  "co-amoxiclav"  , "125mg+500mg"         , "500mg + 125mg"
)

stopifnot(nrow(switched[!correct, on = .(substance, strength)]) == 0L)

abx[correct, on = .(substance, strength), strength := correct]
remove(comb_rel, combinations, switched, correct)


# Split up strength and unit
abx[, strength := str_extract(strength, "^[^ +]*")] 
abx[, unit     := str_extract(strength, "(?<=[:digit:])[a-z].*")]   
abx[, strength := as.numeric(str_extract(strength, "^[:digit:]*"))]

# Convert all values either to mg or to mu
abx[unit %in% c("g", "gram"), 
    `:=`(strength = strength * 1000, unit = "mg")]
abx[unit %in% c("unit"), 
    `:=`(strength = strength / 1000000, unit = "mu")]
abx[str_detect(unit, "mg/[0-9]ml"), `:=`(
  strength = strength / as.numeric(str_extract(unit, "[0-9]{1,2}(?=ml)")), 
  unit = "mg")
]
abx[unit %in% c("mg/ml", "mg/vial"), unit := "mg"]


# Use the name, strength and route to add DDD
ddd_map <-
  read_excel(file.path(lu_dir,"PASS", "DDD.xlsx")) %>% 
  as.data.table()
ddd_map[unit == "g", `:=`(ddd = ddd * 1000, unit = "mg")]

abx[ddd_map, on = c("substance" = "abx", "route"), "ddd" := ddd]

stopifnot(sum(is.na(abx$ddd)) == 0)

abx[, "total_ddd" := .(qty * strength / ddd)]



# Find all possible indications (according to Dolk et al. 2018) -----------
## @knitr indication
#+ indication, include = FALSE

def_indication <- 
  infect_db() %>% 
  # semi_join(def_copd, by = "patid") %>%  # Select all patients instead
  semi_join(def_patients, by = "patid") %>% 
  in_date(study_start %m-% lookback, study_end) %>% 
  select(patid, eventdate, consid, medcode)

indication <- collect_dt(def_indication, convert = TRUE)

# Add read codes
read_dict <- medical() %>% collect_dt()
indication %<>% merge(read_dict, on = "medcode")

# Add classified conditions
resp_class <-
  read_excel(file.path(subfolder, "resp_indication_chapters.xlsx"), 
             col_names = c("code", "type", "desc", "condition", "hier"),
             skip = 1)

con_def <- dolk_2018_conditions_class[!str_detect(code, "^2\\.")]
con_def <- rbind(
  con_def,
  
  # More granular definition for respiratory prescribing
  # than in the default classification
  as.data.table(resp_class)[, .(code, condition)]
)

code_list <- dolk_2018(condition_lbls = con_def)
code_type <- dolk_symptoms_2018()[, .(readcode, type)]

indication[code_list, on = "readcode", 
           c("system", "condition") := .(system_desc, condition)]
indication[code_type, on = "readcode", type := type]
indication %<>% .[!is.na(system)]

remove(read_dict, code_list, resp_class, con_def)




# Obtain comorbidities ----------------------------------------------------
## @knitr comorbs
#+ comorbs, include = FALSE

# Use all other comorbidity code
codes_qof <- 
  codes_qof_db() %>% 
  filter(comorbidity != "copd", 
         !(subclass %in% c("ckd_stage_1", "ckd_stage_2"))) %>% 
  collect_dt()


# Select all records with one of those codes in the study population
# NOTE: exclude COPD (by definition) and CKD stage 1&2 (unreliable coding)
comorb <- 
  qof_db() %>% 
  records_db(codes_qof) %>% 
  # semi_join(def_copd, by = "patid") %>%  # Select all patients instead
  semi_join(def_patients, by = "patid") %>% 
  filter(!is.na(eventdate)) %>% 
  select(-num_rows) %>% 
  collect_dt(convert = TRUE)


# Add the disease labels
comorb[codes_qof, on = "medcode", comorbidity := comorbidity]

# Keep the earliest for each comorbidity
comorb %<>% 
  split(., f = .$comorbidity) %>% 
  map(first_record) %>% 
  map(~ .[, .(patid, eventdate)]) %>% 
  map2(., names(.), ~ setnames(.x, old = "eventdate", new = .y)) %>% 
  reduce(merge, by = "patid", all = TRUE)



# Obtain obesity status ---------------------------------------------------
## @knitr obese
#+ obese, include = FALSE

# Get all explicit obesity diagnoses
obese_diag <- 
  obese_db() %>% 
  # semi_join(def_copd, by = "patid") %>% 
  semi_join(def_patients, by = "patid") %>% 
  collect_dt(convert = TRUE)

obese_codes <- codes_obese_db() %>% collect_dt()
obese_codes[, obese := ifelse(str_detect(description, "40"), 
                              "severely obese", "obese")]


# Get additional BMI measurements (BMI, weight, height)
bmi <- 
  bmi_db() %>% 
  # semi_join(def_copd, by = "patid") %>%  # Select all patients instead
  semi_join(def_patients, by = "patid") %>% 
  collect_dt(convert = TRUE)

bmi[, c("weight", "bmi", "height") := map(.SD, as.numeric), 
    .SDcols = weight:height]

setorder(bmi, patid, eventdate)

# Carry the last height observation forward for each patient
bmi[, height := .(if(is.na(height[1])) c(-Inf, height[-1]) 
                   else  height), by = patid]
bmi[, height := zoo::na.locf(height), by = patid]
bmi[height < 1.3, height := NA]

# When there is no BMI value but weight and (forward-carried) height,
# compute BMI under the assumption that height doesn't change too much
setorder(bmi, patid, eventdate, bmi, na.last = TRUE)
bmi %<>% .[, .SD[1], by = .(patid, eventdate)]
bmi[is.na(bmi), bmi := weight / (height ** 2)]


# Combine both to a measure of obesity

obese <- 
  rbind(
    obese_diag[obese_codes, on = "medcode", nomatch = 0, 
               .(patid, eventdate, obese)],
    bmi[!is.na(bmi), .(patid, eventdate, 
                       obese = case_when(bmi >= 40 ~ "severely obese", 
                                         bmi >= 30 ~ "obese",
                                         TRUE ~ "non"))]
  ) %>% unique()



# Obtain smoking status ----------------------------------------------------
## @knitr smoke
#+ smoke, include = FALSE

smoke <- 
  smoke_db() %>% 
  # semi_join(def_copd, by = "patid") %>% 
  semi_join(def_patients, by = "patid") %>% 
  select(-adid, -data2, -data3, -num_rows) %>% 
  collect_dt(convert = TRUE)

codes_smoke <- codes_smoke_db() %>% collect_dt()

# Use CALIBER smoking algorithm 
# (www.caliberresearch.org/portal/show/smoking_status_gprd)
smoke[codes_smoke, on = "medcode", status := smoke]
smoke[enttype == 4, 
      status := case_when(data1 == "1" & status == "non" ~ "non & smoke",
                          data1 == "1" & status == "ex"  ~ "ex & smoke",
                          data1 == "1" ~ "smoke",
                          data1 == "2" & status == "smoke" ~ "non & smoke",
                          data1 == "2" & status == "ex" ~ "ex",
                          data1 == "2" ~ "non",
                          data1 == "3" & status == "smoke" ~ "ex & smoke",
                          data1 == "3" ~ "ex",
                          TRUE ~ status)]

# Change the ambivalent codes in favour of non-smoking (this is justified,
# as a) it is more conservative and b) there are very general codes that
# are currently counted as smoking, but those could easily just be used 
# with indicating a smoking status (e.g. 137..00 Tobacco consumption)
smoke[status == "non & smoke", status := "non"]
smoke[status == "ex & smoke", status := "ex"]

smoke[, c("enttype", "data1") := NULL]





# Get measures of COPD severity -------------------------------------------
## @knitr fev1
#+ fev1, include = FALSE

# Get FEV1 codes (and associated FVC codes that can be utilised)
codes_fev <- 
  read_excel(path = file.path(lu_dir, "PASS", "copd.xlsx"), 
             sheet = "FEV")

setDT(codes_fev)
codes_fev %<>% 
  .[(qof == "y" | type == "fvc") & type != "other", 
    .(medcode = as.integer(medcode), readterm, measure = type)]

# Select all records in the database associated with one of those codes
fev <- 
  records_db(test_db(), codes_fev) %>% 
  select_cohort() %>% 
  select(-consid, -staffid, -(data4:data8)) %>% 
  collect_dt(convert = TRUE)

fev[, str_c("data", 1:3) := map(.SD, as.numeric), .SDcols = data1:data3]
setnames(fev, c("data1", "data2"), c("op", "value"))
setorder(fev, patid, eventdate)


# Add the unit descriptions for legibility
units <- 
  read_tsv(file.path(lu_dir, "TXTFILES", "SUM.txt"), skip = 1,
           col_names = c("code", "unit"),
           col_types = list(col_integer(), col_character()))
setDT(units)

fev[units, on = .(data3 = code), unit := unit]
fev[, data3 := NULL]


# Restrict to the most common, interpretable units (99.5% of records)
fev %<>% .[unit %in% c("%", "L", "ratio", "No Data Entered")]
fev[unit == "ratio", unit := "%"] # treat them interchangably

# Add the type of measurement
fev[codes_fev, on = "medcode", measure := measure]
fev[codes_fev, on = "medcode", description := readterm]


# Define which measurements can be used and which can't
fev %<>% split(., .$measure)
fev[["predict fev/fvc"]] <- NULL

range_L <- list(0.1, 7)
range_perc <- list(5, 100)

# Define valid values for FEV1
fev$fev %<>% 
  .[value %between% range_L & unit %in% c("L", "No Data Entered") |
    value < min(unlist(range_perc))]


# Define valid values for FEV1/FVC
fev$`fev/fvc` %<>% 
  .[value %between% range_perc & unit %in% c("%", "No Data Entered") |
    value %between% list(max(unlist(range_L)), max(unlist(range_perc)))]


# Define valid values for FVC
fev$fvc %<>% 
  .[value %between% range_L & unit %in% c("L", "No Data Entered")]


# Define valid values for FEV1 as % of predicted
fev$`predict fev` %<>% 
  .[value %between% range_perc & unit %in% c("%", "No Data Entered") |
    value %between% range_perc & unit == "L" ] # double check this line


fev %<>% map(~ .[op == 3]) # Limit to equal operators (only a handful)


predict_fev1 <- function(sex, age, height){
  # Calculate the predicted FEV1 volume based on age, sex and height
  #
  # Args:
  #   sex - vector of 0 (male) and 1 (female)
  #   age - vector of age in years
  #   height - vector of height in cm
  #
  # Result:
  #   vector with predicted fev1 values
  
  coefs <- matrix(c(- 2.49, - 0.029, 0.043,   # male
                    - 2.60, - 0.025, 0.0395), # female
                  byrow = TRUE, ncol = 3)
  
  pred <- coefs %*% rbind(1L, age, height)
  
  ifelse(sex == 0, pred[1, seq_along(sex)], pred[2, seq_along(sex)])
}


# Add variables needed to predict FEV1
fev %>% 
  walk(~ .[, .(patid, eventdate, medcode, value)]) %>% 
  walk(~ .[cohort, on = "patid", 
           `:=`(sex = female, 
                age = year(eventdate) - year(birth_date))])

heights <- bmi[!is.na(height), .(patid, eventdate, height)]

fev %<>% 
  map(~ heights[., on = .(patid, eventdate), roll = TRUE, rollends = TRUE])

fev %<>% walk(~ .[, pred_fev := predict_fev1(sex, age, height * 100)])
fev %<>% walk(setorder, patid, eventdate, value)


# For each date with a measurement, choose the best estimate of FEV1
# (i.e. the one with the least calculation necessary)

fev <- rbind(
  # If available, use the % of predicted FEV1 as input by the clinician 
  fev$`predict fev`[, .(patid, eventdate, fev = value, method = 1L)],
  
  # where no directly recorded predicted FEV1 is available, use the 
  # FEV1 in liter and divide by the predicted FEV1 as calculated above
  # (ignore values where the entered FEV1 is larger than a FVC on the
  # same day)
  fev$fev[!fev$fvc, on = .(patid, eventdate, value > value)] %>% 
        .[, .(patid, eventdate, fev = value / pred_fev * 100, method = 2L)],
  
  # where no FEV1 is available, look if there is FEV1/FCV and FCV recorded.
  # If so, use those to calculate first FEV1 and then % of predicted.
  fev$`fev/fvc`[(fev$fvc[, .(patid, eventdate, fvc = value)]), 
                on = .(patid, eventdate), nomatch = 0, 
                .(patid, eventdate, fev = value * fvc / pred_fev, method = 3L)]
)

setorder(fev, patid, eventdate, method)
fev %<>% .[fev %between% range_perc]
fev %<>% .[!is.na(fev), .SD[1], by = .(patid, eventdate)]



# MRC breathlessness scale ------------------------------------------------
## @knitr mrc
#+ mrc, include = FALSE

codes_mrc <- 
  read_excel(path = file.path(lu_dir, "PASS", "copd.xlsx"), 
             sheet = "MRC")


codes_mrc %<>% as.data.table() %>% .[qof == "y", .(medcode, readterm)]
codes_mrc[, medcode := as.integer(medcode)]

codes_mrc[, mrc := str_extract(readterm, "(?<=grade )[:digit:]")]

# Select all records in the database associated with one of those codes

mrc <- 
  records_db(clinical_db(), codes_mrc) %>% 
  select_cohort() %>% 
  select(patid, eventdate, medcode) %>% 
  collect_dt(convert = TRUE)

mrc[codes_mrc, on = "medcode", mrc := mrc]




# Influenza vaccination ---------------------------------------------------
## @knitr influenza_vacc
#+ influenza_vacc, include = FALSE

codes_flu <- 
  read_excel(path = file.path(lu_dir, "PASS", "copd.xlsx"), 
             sheet = "flu vaccine")


codes_flu %<>% as.data.table() %>% .[qof == "y", .(medcode, readterm)]
codes_flu[, medcode := as.integer(medcode)]

# Select all records in the database associated with one of those codes

clin_or_imm_db <- 
  dplyr::union(
    clinical_db() %>% select(patid, eventdate, medcode),
    immun_db() %>% select(patid, eventdate, medcode)
  )

flu_vacc <- 
  records_db(clin_or_imm_db, codes_flu) %>% 
  # semi_join(def_copd, by = "patid") %>% 
  semi_join(def_patients, by = "patid") %>% 
  collect_dt(convert = TRUE)




# Get the number of consultations per patient -----------------------------
## @knitr consultations
#+ consultations, include = FALSE

# Get all consultations arising from the cohort during the observations
# See Kontopantelis et al. (2015)
face2face <- dtribble(
  ~ constype , ~ type,  ~ cons,
  1          , "face", "Clinic",
  2          , "face", "Night visit, Deputising service",
  3          , "face", "Follow-up/routine visit",
  4          , "face", "Night visit, Local rota",
  6          , "face", "Night visit , practice",
  7          , "face", "Out of hours, Practice",
  8          , "face", "Out of hours, Non Practice",
  9          , "face", "Surgery consultation",
  11         , "face", "Acute visit",
  24         , "face", "Children's Home Visit",
  27         , "face", "Home Visit",
  28         , "face", "Hotel Visit",
  30         , "face", "Nursing Home Visit",
  31         , "face", "Residential Home Visit",
  32         , "face", "Twilight Visit",
  34         , "face", "Walk-in Centre",
  36         , "face", "Co-op Surgery Consultation",
  37         , "face", "Co-op Home Visit",
  40         , "face", "Community Clinic",
  50         , "face", "Night Visit",
  
  10         , "tele", "Telephone call from a patient",
  21	       , "tele", "Telephone call to a patient",
  35	       , "tele", "Co-op Telephone advice",
  55	       , "tele", "Telephone Consultation"
)

def_consultations <-
  consult_db() %>% 
  in_date(study_start %m-% lookback, study_end) %>% 
  # semi_join(def_copd, by = "patid") %>% 
  semi_join(def_patients, by = "patid") %>% 
  select(patid, eventdate, constype, consid)

cons <- collect_dt(def_consultations, convert = TRUE)

# Limit to face-to-face
cons %<>% .[(face2face[, !("cons")]), on = "constype"]
setorder(cons, patid, eventdate, type)

# Limit to one consultation per day per patient
cons %<>% .[, .SD[1], by = .(patid, eventdate)]
cons[, c("constype", "consid") := NULL]




# Obtain all information on hospital utilisation --------------------------
## @knitr hospital
#+ hospital, include = FALSE

deduplicate_admission <- function(dt){
  # Some of the admissions in HES are nested within bigger spells (maybe ward move?),
  # or are overlapping. This function removes episodes that are completely nested in 
  # a bigger spell and shortens thoe that overlap
  #
  # Args:
  #   dt - data.table with hospital admissions
  #
  # Result:
  #   a deduplicated hospital admission data.table
  
  dt <- copy(dt)
  dt[, id := 1:nrow(dt)]
  
  nested <- 
    dt[dt, .(patid, admidate, discharged, id = i.id), nomatch = 0,
       on = .(patid, admidate   <= admidate, 
              discharged >  discharged)]
  
  overlap <-
    dt[dt, .(patid, overlap = discharged, id), nomatch = 0,
       on = .(patid, admidate   < admidate, 
              discharged > admidate, 
              discharged <= discharged)]
  
  dt[overlap, on = .(id), 
     `:=`(discharged = overlap, 
          duration = as.integer(time_length(admidate %--% overlap, unit = "days")))]
  dt[!nested, on = .(id), !("id")]
}


# Get the inpatient information
def_admission <-
  emergency_db() %>% 
  in_date(study_start %m-% lookback, study_end) %>% 
  # semi_join(def_copd, by = "patid") %>% 
  semi_join(def_patients, by = "patid") %>% 
  select(patid, spno, admidate, discharged, duration)

admission <- collect_dt(def_admission, convert = TRUE)
admission <- deduplicate_admission(admission)


def_hosp_diag <- 
  hosp_diag_db() %>% 
  select_cohort() %>% 
  select(patid, spno, icd)

hosp_diag <- collect_dt(def_hosp_diag)


# Get the A&E information
def_ae <-
  ae_db() %>% 
  in_date(study_start %m-% lookback, study_end) %>% 
  # semi_join(def_copd, by = "patid") %>% 
  semi_join(def_patients, by = "patid") %>% 
  select(patid, arrivaldate)

ae <- collect_dt(def_ae, convert = TRUE)
ae <- unique(ae)

remove(def_admission, def_hosp_diag, def_ae)






# Additional Rothnie/Quint information ------------------------------------

# Add AECOPD specific codes (taken from Rothnie/Quint et al. 2016)
infect_list <- tbl(get_db_conn(), "codes_infect") %>% collect_dt()
roth_list <- rothnie_2016() %>% rbindlist()
not_in_dolk <- roth_list[!infect_list, on = "medcode"]

def_add_indication <- 
  clinical_db() %>% 
  records_db(not_in_dolk, id = "medcode") %>% 
  # semi_join(def_copd, by = "patid") %>%  # Select all patients instead
  semi_join(def_patients, by = "patid") %>% 
  in_date(study_start %m-% lookback, study_end) %>% 
  select(patid, eventdate, consid, medcode)

add_indication <- collect_dt(def_add_indication, convert = TRUE)


# Identify prednisone 5mg for rescue packs --------------------------------
## @knitr prednisolone
#+ prednisolone, include = FALSE


# Definition following Rothnie/Quint et al. 2016
codes_predni <- rothnie_2016(type = "prodcode")[["OCS"]]

def_prednisolone <- 
  therapy_db() %>% 
  select_cohort() %>% 
  records_db(codes_predni, id = "prodcode") %>% 
  in_date(study_start %m-% lookback, study_end) %>% 
  select(patid, prescdate = eventdate, prodcode, qty, ndd)

prednisolone <- collect_dt(def_prednisolone, convert = TRUE)




# Identify moderate acute exacerbations based on Rothnie et al. 2016 ------
## @knitr aecopd
#+ aecopd, include = FALSE

# Use algorithms 3, 5, 6, 8, and 12 from Rothnie et al. 2016

r_diag_list <- rothnie_2016()[c("LRTI", "AECOPD")]
r_symp_list <- rothnie_2016()[c("Breathlessness", "Cough", "Sputum")]
r_abx_list <- rothnie_2016(type = "prodcode")[["Antibiotics"]]

r_abx <- abx[(r_abx_list[, "prodcode"]), on = "prodcode", nomatch = 0]
r_abx_restr <- r_abx[ndd != 0 & (qty / ndd) %between% list(5, 14)]

r_ocs <- prednisolone
r_ocs_restr <- prednisolone[ndd != 0 & (qty / ndd) %between% list(5, 14)]

full_ind <- 
  rbind(
    indication[, .SD, .SDcols = names(add_indication)],
    add_indication
  )

r_symp <-   
  r_symp_list %>% 
  map(~ .[, "medcode"]) %>% 
  map(merge, y = full_ind, by = "medcode") %>% 
  map2(names(.), ~ .x[, .(patid, eventdate, type = .y)]) %>% 
  map(unique) %>% 
  reduce(merge, by = c("patid", "eventdate"), all = TRUE)

r_symp[, count := rowSums(!is.na(.SD)), .SDcols = 3:5]
r_symp %<>% .[count >= 2, .(patid, eventdate)] 

# Algorithm 3: Antibiotics and OCS (both length 5-14 days) on same day
alg_3 <- 
  r_abx_restr[r_ocs_restr, on = .(patid, prescdate), nomatch = 0,
              .(patid, eventdate = prescdate)]
alg_3 %<>% unique()

# Algorithm 5: 2 Symptoms and OCS on same day
alg_5 <- 
  r_symp[r_ocs, on = .(patid, eventdate = prescdate), nomatch = 0,
         .(patid, eventdate)]
alg_5 %<>% unique()

# Algorithm 6: 2 Symptoms and antibiotics on same day
alg_6 <- 
  r_symp[r_abx, on = .(patid, eventdate = prescdate), nomatch = 0,
         .(patid, eventdate)]
alg_6 %<>% unique()

# Algorithms 8 and 12: LRTI and AECOPD code alone
alg_8_12 <- 
  full_ind[rbindlist(r_diag_list), on = "medcode", nomatch = 0] %>% 
  .[, .(patid, eventdate)]
alg_8_12 %<>% unique()


# Combine all algorithms to define moderate aecopds
aecopd <- rbindlist(mget(ls(pattern = "alg_"))) %>% unique()


remove(r_diag_list, r_symp_list, r_abx_list,
       r_abx, r_abx_restr, r_ocs, r_ocs_restr, 
       full_ind,
       alg_3, alg_5, alg_6, alg_8_12)



# Identify severe acute exacerbations based on Rothnie et al. 2016 --------
## @knitr severe_aecopd
#+ severe_aecopd, include = FALSE

sev_ae_def <- dtribble(
  ~ icd  , ~ description,
  "J22"  , "LRTI"       ,
  "J44.0", "AECOPD"     ,
  "J44.1", "AECOPD"     
)

severe_aecopd <- 
  hosp_diag[sev_ae_def, on = "icd", nomatch = 0] %>% 
          .[admission, on = .(patid, spno), nomatch = 0]

severe_aecopd[, c("spno", "icd", "description", "duration") := NULL]


# Save all derived datasets -----------------------------------------------
## @knitr save
#+ save, include = FALSE

mget(c("practices", "patients", "cohort", "copd", "abx", "indication",  
       "comorb", "obese", "smoke", "fev", "mrc", "flu_vacc", 
       "admission", "hosp_diag", "ae", "cons",
       "add_indication", "prednisolone", "aecopd", "severe_aecopd")) %>% 
  walk2(., names(.), save_derived, compress = "gz")



