###########################################################################
# Author:   Patrick Rockenschaub
# Project:  Preserve Antibiotics through Safe Stewardship (PASS)
#           Primary Care work package 1
#           COPD analysis
#
# File:     02_define_indication.R
# Date:     29/01/2019
# Task:     Use the extracted records and apply all classifications for the
#           possible reasons of prescribing
#
###########################################################################


# Path from project directory to this file
# NOTE: must be set in each programme separately
subfolder <- "04_copd"

# Initialise the workspace
source(file.path(subfolder, "00_init.R"))


# Load additional functionality to compute prescribing sequences
source("04_repeat_prescriptions.R")




char(abx, indication, add_indication, prednisolone, aecopd) %>% 
  walk(load_derived)

indication <- copy(indication) # Fix data.table allocation error



# Identify all nitrofurantoin ---------------------------------------------
## @knitr nitro
#+ nitro, include = FALSE

# as nitrofurantoin is only indicated for UTI
nitrofurantoin <- abx[substance == "nitrofurantoin",
                      .(patid, abx_id, prescdate)]

reason_nitro <- nitrofurantoin[, .(patid, abx_id)]
reason_nitro[, system := "urogenital tract"]
reason_nitro[, condition := "urinary tract"]
reason_nitro[, criteria := "nitro"]




# Identify through Rothnie et al. algorithm -------------------------------
## @knitr aecopd
#+ aecopd, include = FALSE

reason_aecopd <- 
  abx[aecopd, on = .(patid, prescdate = eventdate), nomatch = 0] %>% 
    .[, .(patid, abx_id, system = "rt/ent", condition = "copd related")]
reason_aecopd[, criteria := "aecopd"]



# Map the most likely indication ------------------------------------------
## @knitr reason
#+ reason, include = FALSE

small <- abx[, .(patid, abx_id, prescdate, consid)]

# Option 1: indication coded on the same day
on_day <- indication[small, on = .(patid, eventdate = prescdate), nom = 0]
on_day[, i.consid := NULL]
on_day[, criteria := "same day"]

# Option 2: prescription later, but has the same consultation identifier
prev_cons <- 
  indication[small, on = .(patid, eventdate < prescdate, consid), nom = 0]
prev_cons[, criteria := "prev cons"]

# Combine the options in ascending order (first 1, if not then 2)
reason <- rbind(
  on_day, 
  prev_cons[!on_day, on = "abx_id"]
)

collapse_conditions <- function(ind_dt){
  # Group multiple conditions within one system on one day together
  # in a list
  #
  # Args:
  #   ind_dt - data.table of the same format as indication above
  #
  # Result:
  #   a collapsed data.table
  
  ind_dt %<>% 
    .[, .(conditions = list(unique(condition))), 
      by = .(patid, abx_id, system, type, criteria)]
  
  # Take the highest level of evidence per consulationt (i.e. if diagnosis
  # is available, choose diagnosis, otherwise go with symptom)
  ind_dt[, best := min(type), by = abx_id]
  ind_dt %<>% .[type == best, !(c("type", "best"))]
}

reason %<>% collapse_conditions()





# Evaluate the dominant system for each episode ---------------------------
## @knitr systems
#+ systems, include = FALSE

# All prescriptions with one specific body system (i.e. not miscellaneous)
specific <- 
  reason[system != "misc"] %>%  
  .[, .(patid, abx_id, system, conditions, criteria)] %>% 
  unique()

# Subset of prescriptions with two specific body systems coded
multiple <- 
  specific[, .(.N, conditions = list(unlist(conditions))), 
           by = .(patid, abx_id, criteria)] %>% 
  .[N > 1, !("N")] %>% 
  .[, .(patid, abx_id, system = "multiple systems",
        conditions, criteria)]

# All prescriptions with only miscellaneous codes
misc <- 
  reason[!specific, on = "abx_id"] %>% 
  .[system == "misc"] %>% 
  .[, .(patid, abx_id, system, conditions, criteria)] %>% 
  unique()

# Combine all and collapse some of the labels
reason <- 
  rbind(
    specific[!multiple, on = "abx_id"],
    multiple, 
    misc
  ) 

remove(specific, multiple, misc)


# Combine the classifications in order

reason_nitro[, conditions := map(condition, ~ .)]
reason_aecopd[, conditions := map(condition, ~ .)]
reason[, criteria := "codes"]


add_reason <- function(reason, new){
  # Add new rows to the reason data.table, but make sure that there
  # was no indication picked up with an earlier algorithm
  #
  # Args:
  #   reason - data.table with existing indications
  #   new - data.table (same columns) with new indications
  #
  # Result:
  #   Combined data.table
  
  rbind(reason, new[!reason, on = "abx_id"])
}


reason <- 
  add_reason(reason_nitro[, !("condition")], reason_aecopd[, !("condition")]) %>% 
  add_reason(reason)



# Classify acute (non-continuous) prescribing -----------------------------
## @knitr acute
#+ acute, include = FALSE

rule <- substitute(system == i.system | (is.na(system) | is.na(i.system)) & 
                                         substance == i.substance)
re_presc <- list(compare = rule, within = 30)

acute <- 
  merge(abx[issueseq == 0], reason[, .(abx_id, system)], 
        by = "abx_id", all.x = TRUE) %>% 
    .[order(patid, prescdate), 
      .(patid, abx_id, prescdate, system, substance,
        window = prescdate %m-% days(re_presc$within))]

# See if there was another prescription before
acute_re <- 
  acute[(acute[, !("window")]), nomatch = 0,
        on = .(patid, window <= prescdate, prescdate > prescdate)]

acute_re %<>% .[eval(re_presc$compare)] # Keep only represcriptions
acute_re %<>% .[, .(earlier = max(i.abx_id)), by = abx_id] # Latest abx id

hash <- ""
# Link to the earliest prescription in the sequence of resprescriptions
while(hash != digest::sha1(acute_re$earlier)){ # stop if no change anymore
  hash <- digest::sha1(acute_re$earlier) # hash to test for change
  
  # Propagate the links, moving up one level
  acute_re[acute_re, on = .(earlier = abx_id), earlier := i.earlier]
}

# Count the number of prescriptions in the sequence after each initial prescr.
acute_re[, N := .N, by = earlier]

# If only one represcribing, call that one `second`
acute[(acute_re[N == 1]), on = .(abx_id), as := "second"]

# If more than two prescriptions, call all in the sequence `short-term`
# (including the first)
acute[(acute_re[N > 1]), on = .(abx_id), as := "short-term"]
acute[(acute_re[N > 1]), on = .(abx_id = earlier), as := "short-term"]

# Everything not labelled yet is an initial prescription
acute[is.na(as), as := "first"]

remove(acute_re)




# Classify non-acute prescribing ------------------------------------------
## @knitr non_acute
#+ non_acute, include = FALSE

non_acute <- list(treat = 162, out_of = 180)

# Impute the lengths using a baseline of 1 (most liberal imputation)
abx[, ndd_01 := if_else(ndd == 0, 1, ndd)]

# Impute the lenghts using median imputation, 
# based on substance, strength and quantity
med_len_ssq <- 
  abx[ndd != 0, .(ndd_med = median(ndd)), by = .(substance, strength, qty)]
med_len_ss <-
  abx[ndd != 0, .(ndd_med = median(ndd)), by = .(substance, strength)]
med_len_s <- 
  abx[ndd != 0, .(ndd_med = median(ndd)), by = substance]

med_len <- 
  med_len_ssq[unique(abx[, .(substance, strength, qty)]), 
              on = .(substance, strength, qty)] %>% 
  .[med_len_ss, on = .(substance, strength), 
    ndd_med := if_else(is.na(ndd_med), i.ndd_med, ndd_med)] %>% 
  .[med_len_s, on = "substance", 
    ndd_med := if_else(is.na(ndd_med), i.ndd_med, ndd_med)]

med_len[is.na(ndd_med), ndd_med := 1]

abx[med_len, on = .(substance, strength, qty), 
    ndd_med := if_else(ndd == 0, ndd_med, ndd)]




repeat_expl <- identify_explicit_repeats(abx)
setnames(repeat_expl, "issue", "seq") # Leave for backwards compatibility

repeat_6m <- identify_6m_repeats(abx)
repeat_dur <- 
  do.call(identify_duration_repeats, 
          args = c(list(abx = abx, use = substitute(qty / ndd_med)), 
                   non_acute))


abx[acute, on = "abx_id", as := as]
abx[repeat_expl, on = "abx_id", as := "short-term"]
abx[repeat_6m, on = "abx_id", as := "long-term"]
abx[repeat_dur, on = "abx_id", as := "long-term"]

presc_as <- abx[, .(patid, abx_id, as)]


# Sensitivity measures
# long_ddd <- 
#   do.call(identify_duration_repeats, args = c(list(abx = abx), non_acute))
# 
# long_01 <- 
#   do.call(identify_duration_repeats, 
#           args = c(list(abx = abx, use = substitute(qty / ndd_01)), 
#                    non_acute))




# Add additional indications using prescribing sequences ------------------
## @knitr seq_indication
#+ seq_indication, include = FALSE

# Following the second algorithm outlined in Dolk et al. (2018), assign 
# additional indications for prescriptions with missing 

# 1) Indication on the same day as the start of the prescribing sequence
seq_start <- rbind(repeat_expl, repeat_6m, repeat_dur)
seq_start[, seq := NULL]
setorder(seq_start, abx_id, start_date)

seq_start %<>% .[, .SD[1], by = abx_id]
seq_start[abx, on = .(abx_id), `:=`(patid = patid, substance = substance)]
seq_start[abx, on = .(patid, start_date = prescdate, substance), 
          `:=`(abx_seq_start = i.abx_id)]

seq_start[, criteria := "sequence"]
seq_start[reason, on = .(abx_seq_start = abx_id), 
          `:=`(system = system, conditions = conditions)]
seq_start[, c("start_date", "substance", "abx_seq_start") := NULL]


reason %<>% add_reason(seq_start[!is.na(system)])

remove(seq_start)


# 2) indications associated with the same antibiotic 30 days before
same_30 <- reason[, .(patid, abx_id, system, conditions)]
same_30[abx, on = "abx_id", `:=`(prodcode = prodcode, 
                                 prescdate = prescdate)]
same_30[, `:=`(lower = prescdate %m+% days(1),
               upper = prescdate %m+% days(30))]


same_30[(abx[!reason, on = "abx_id"]), 
        on = .(patid, prodcode, lower <= prescdate, upper >= prescdate),
        follow_id := i.abx_id]
same_30 %<>% .[!is.na(follow_id)]

setorder(same_30, follow_id, -lower)
same_30 %<>% .[, .SD[1], by = follow_id] # Take the latest predecessor


same_30[, c("abx_id", "prodcode", "prescdate", "lower", "upper") := NULL]
setnames(same_30, "follow_id", "abx_id")
same_30[, criteria := "30 days prior"]

reason %<>% add_reason(same_30)

remove(same_30)


# 3) indications within 7 days earlier
small[, week := prescdate %m-% days(7)]
days_8 <- 
  indication[small, nom = 0, on = .(patid, consid, eventdate < prescdate, 
                                    eventdate >= week)]
days_8[, eventdate.1 := NULL]

days_8[, max_date := max(eventdate), by = abx_id]
days_8 %<>% .[eventdate == max_date]
days_8[, criteria := "8 days prior"]

days_8 %<>% collapse_conditions()

# Label multiple systems accordingly
days_8[, num_sys := .N, by = abx_id]
days_8 %<>% .[system != "misc" | num_sys == 1]
days_8[, num_sys := .N, by = abx_id]
days_8[num_sys > 1, system := "multiple systems"]
days_8 %<>% .[, .(conditions = list(unique(unlist(conditions)))), 
              by = .(patid, abx_id, system, criteria)]


reason %<>% add_reason(days_8)







# Identify potential prophylaxis ------------------------------------------
## @knitr prophylaxis
#+ ropylaxis, include = FALSE


prophylaxis <- function(def){
  # Identify prophylactic treatement based on a definition consisting of 
  # substance and maximum strength per unit to be usable for prophylaxis.
  # Only long-term treatment is considered prophylactic.
  #
  # Args:
  #   def - data.table with columns substance and proph, where proph is a
  #         list of strenghts for this substance that are recommended for 
  #         prophylaxis
  #
  # Result:
  #   a data.table with the subset of `abx` that agrees with prophylaxis
  
  def[, max_stre := map_int(proph, max)]
  
  # Find all prescriptions that could be prophylaxis
  proph <- def[(abx[as %in% c("long-term")]), 
               on = .(substance, max_stre >= strength), nom = 0]
  
  setnames(proph, "max_stre", "strength")
  setcolorder(proph, c(names(abx), "proph"))
  
  # Remove prescriptions where the number of tablets per day doesn't match
  # the maximum prophylactic dose
  proph[def, on = "substance", max_stre := max_stre]
  proph %<>% .[ndd * strength <= max_stre]
  
  # See if the prophylaxis goes on long enough
  proph[, assumed := map2_int(strength, proph, ~ .y[which.min(abs(.y - .x))])]
  proph[, length := .(qty * strength / assumed)][]
  
  proph[, .(patid, abx_id, criteria = "prophylaxis")]
}


# Source: cks.nice.org.uk/urinary-tract-infection-lower-women
uti_abx <- dtribble(
  ~ substance      , ~ proph     ,
  #----------------|-------------#
  "trimethoprim"   , c(100L)     ,
  "nitrofurantoin" , c(50L, 100L)   
)

uti_proph <- prophylaxis(uti_abx)
uti_proph[, `:=`(system = "urogenital tract", 
                 conditions = list("urinary tract"))]


# Source: bnf.nice.org.uk/treatment-summary/rosacea-and-acne.html
acne_abx <- dtribble(
  ~ substance      , ~ proph     ,
  #----------------|-------------#
  "oxytetracycline", c(500L)     ,
  "tetracycline"   , c(500L)     ,
  "lymecycline"    , c(408L)     ,
  "minocycline"    , c(100L)
)

acne_proph <- prophylaxis(acne_abx)
acne_proph[, `:=`(system = "skin and wounds", 
                  conditions = list("acne"))]


# Source: www.nejm.org/doi/full/10.1056/NEJMoa1104623
copd_abx <- dtribble(
  ~ substance      , ~ proph     ,
  #----------------|-------------#
  "azithromycin"   , c(250L)     
)

copd_proph <- prophylaxis(copd_abx)
copd_proph[, `:=`(system = "rt/ent", 
                  conditions = list("copd related"))]



# Add those to the indications, but only if it doesn't contradict recorded
# indications
reason %<>% add_reason(uti_proph)
#reason %<>% add_reason(acne_proph) # Update: do not infer acne
reason %<>% add_reason(copd_proph)




# Save all derived datasets -----------------------------------------------
## @knitr save
#+ save, include = FALSE

mget(c("reason", "presc_as", "aecopd")) %>% 
  walk2(., names(.), save_derived, compress = "gz")

