#--------------------------------------------------------------------------
#
# Program: 00_utilities.R
# Author:  Patrick Rockenschaub
# Date:    06/12/2017
#
# Purpose: Provide variables and tools that are generally needed throughout
#          the project
#
#--------------------------------------------------------------------------

library(magrittr)
library(tibble)
library(tidyr)
library(dplyr)
library(dbplyr)
library(stringr)
library(purrr)
library(data.table)
library(lubridate)
library(readr)
library(readxl)


# Common functions --------------------------------------------------------

bl <- function(x, ...){
  # Wrapper around str_c to be easily able to write a string over multiple
  # without creating newline characters
  #
  # Args:
  #   x   - character vector to collapse
  #   ... - further character vectors to collapse or parameters that shall
  #         be passed on to str_c
  #
  # Result:
  #   Character vector, the same result as would be obtained from str_c
  
  str_c(x, ..., sep = " ", collapse = " ")
}


enumerate <- function(x){
  # Collapse a character vector into a enumeration with Oxford comma
  #
  # Args: 
  #   x - character vector
  #
  # Return:
  #   character vector of length 1
  
  if(length(x) == 1){
    return(x)
  } else if(length(x) == 2){
    return(str_c(x, collapse = " and "))
  }
  
  str_c(x[-length(x)], collapse = ", ") %>% 
    str_c(str_c("and", x[length(x)], sep = " "), sep = ", ")
}


age_at <- function(date, dob, format = "%Y-%m-%d"){
  # Calculate the age in years for one or more patients.
  #
  # Args:
  #   date   - vector of date objects or character strings representing 
  #            dates at which the age should be calculated
  #   dob    - vector of date objects or character strings representing 
  #            dates at which patients were born
  #   format - additional specification of the date format if character 
  #            representations were provided
  #
  # Result:
  #   Numeric vector containing the year difference between the each date  
  #   of birth and the provided cut-off date(s).
  
  if(is.character(date)){
    date <- parse_date(date, format)
  }
  
  if(is.character(dob)){
    dob <- parse_date(dob, format)
  }
  
  if(!is.Date(date) || !is.Date(dob)){
    stop(bl("Both parameters 'date' and 'dob' must either be datatypes",
            "Date or character vectors that can be converted to dates."))
  }
  
  interval(dob, date) / years(1)
}


prty <- function(x, digits = 0){
  # Round numbers and convert to character for table display
  #
  # Args:
  #   x - numeric vector
  #   digits - number of digits to round to
  #
  # Result:
  #   pretty character vector to display
  
  ifelse(is.na(x), 
         NA_character_,
         base::format(round(x, digits), nsmall = digits, big.mark = ","))
}


`%` <- function(nom, denom, digits = 1){
  # Simple wrapper for formatting percentage calculations
  #
  # Args: 
  #   nom - a numeric vector with all nominators
  #   denom - a numeric vector with all denominators
  #   digits - the number of decimal places in the result
  #
  # Result:
  #   A numeric vector of percentages
  
  round(nom / denom * 100, digits)
}

perc <- `%`


`n_%` <- function(nom, denom, digits = 1){
  # Calculate number of cases and % for table display
  #
  # Args:
  #   nom - nominator vector (used to get N and %)
  #   denom - denominator vector (used to get %)
  #   digits - number of digits to round the percentage to 
  #            (N is always rounded to integer)
  # 
  # Result:
  #   pretty character vector to display
  
  perc <- format(`%`(nom, denom, digits), nsmall = digits)
  
  paste0(prty(nom), " (", perc,  ")")
}

n_perc <- `n_%`


dtribble <- function(...){
  # Convenient wrapper to call tribble() and obtain a data.table
  #
  # Args:
  #   see ?tribble
  #
  # Result:
  #   data.table
  
  as.data.table(tribble(...))
}





load_data <- function(path, datasets){
  # Wrapper around load to load multiple datasets simultaneously
  #
  # Args: 
  #   - character vector with path to the directory
  #   - character vector with the name of the dataset (no file extension)
  #
  # Result:
  #   NULL
  
  d <- !map_lgl(datasets, exists, envir = globalenv())
  
  if(any(!d)){
    warning(bl("Some datasets were not loaded, because they already",
               "existed in the global environment"))
  }
  
  file.path(path, str_c(datasets[d], ".Rdata")) %>% 
    walk(load, envir = globalenv())
}



comorbidity_matrix <- function(comorb_df, ref_date){
  # Bring a list of comorbidity events (disease, pat_id, eventdate, 
  # medcode, category) in long format into a comorbidity matrix 
  # with one row per patient and an indicator for each comorbidity.
  # The reference date will be used to identify patients with the
  # co-morbidity.
  #
  # Args:
  #   comorb_df - data.table containing all comorbidity events
  #   ref_date  - Reference date; can either be one date for all 
  #               patients (e.g. study start) or an individual date
  #               for each patient. In the latter case, a dataframe
  #               containing (pat_id, ref_date) must be provided.
  #
  # Result:
  #   data.table with one row per patient and a logical column for 
  #   each comorbidity.
  
  if(class(ref_date) == "Date" && length(ref_date) == 1){
    comorb_df$ref_date <- ref_date
  } else if (is.data.table(ref_date) && !is.null(ref_date[["patid"]])){
    comorb_df <- comorb_df[ref_date, on = "patid"]
  } else {
    stop(bl("Reference date (ref_date) must either be a single date or ", 
            "a data.table with a separate dates for each patient."))
  } 
  
  comorb_df <- 
    comorb_df[eventdate < ref_date, .(patid, disease, flag = TRUE)] %>% 
    distinct() %>% 
    dcast(patid ~ disease, value.var = "flag")
  
  comorb_df[is.na(comorb_df)] <- FALSE
  comorb_df
}



extract_obs <- function(cohort, data){
  # Get all observations from some dataset that came from a patient.
  # The cohort must contain variables enter_date and leave_date. Only rows
  # with an eventdate between those dates are included. 
  #
  # Args:
  #   cohort - data.frame containing one row for each included patient
  #            together with the fields patid, pracid, enter_date and 
  #            leave_date
  #   data   - data.frame containing the data of interest, together with 
  #            a patid and an eventdate field
  #
  # Result:
  #   data.frame structured like data, but with additional pracid column 
  
  merge(cohort, data, by = "patid") %>% 
    .[eventdate %>% data.table::between(enter_date, leave_date)] %>% 
    .[, c("pracid", names(data)), with = FALSE]
}



# Model summarisiation ----------------------------------------------------

model_coef <- function(model, vars = "all", rel = TRUE){
  # Get the coefficients and confidence intervals for a negative binomial 
  # model. Coefficients are on the scale of the response variable
  #
  # Args: 
  #   model - a result object from lm, glm, glmer, glm.nb or glmer.nb
  #   vars  - variables for which the coefficient should be extracted
  #   rel   - shall the coefficients be relative to the intercept
  #
  # Result:
  #   data.table with variable name, the estimate and upper/lower CIs
  
  summ <- data.table::as.data.table(broom::tidy(model))
  
  if(!rel){
    summ[term != "(Intercept)"]$estimate <-
      summ[term != "(Intercept)"]$estimate + 
      summ[term == "(Intercept)"]$estimate
  }
  
  summ[, "low" := (estimate + qnorm(0.025)*std.error)]
  summ[, "high" := (estimate + qnorm(0.975)*std.error)]
  
  if(length(vars) == 1 && vars == "all") vars <- summ$term
  
  summ[
    grep(paste(vars, collapse = "|"), term),
    c(var = .(term), lapply(.SD, function(x) round(exp(x), 2))),
    .SDcols = c("estimate", "low", "high")]
}

ci <- function(coefs, rel = TRUE){
  # Take the output from model_coef() and combine the estimate and CIs 
  # into a printable character column.
  #
  # Args: 
  #   coefs - output from model_coef()
  #   rel   - shall the base class be set to 1
  #
  # Result:
  #   a one column data.table containing the character representation
  
  col <- coefs[, .("ci" = paste0(estimate, " (", low, "-", high, ")"))]
  if(rel) col[coefs$var == "(Intercept)"]$ci <- "1"
  col
}






# Parallelize -------------------------------------------------------------

parallelize <- function(.x, .f, ..., .libs = c("data.table", "dplyr")){
  # Parallelize naive problems that can be independently computed for each 
  # patient. 
  #
  # NOTE: Does not necessarily keep the order in the data.table. Make sure
  #       that the function gets the data in the right shape itself.
  # 
  # Args: 
  #   .x - data.table containing the data. MUST HAVE patid
  #   .f - function to be applied to the data.table
  #   ...- any further parameters passed on to the function
  #   .libs - libraries necessary for the function, as character vector
  #
  # Result:
  #   data.table with the result
  
  library(parallel)
  
  cores <- detectCores() - 1
  cl <- makeCluster(cores)
  on.exit(stopCluster(cl))
  
  clusterCall(cl, .libs, fun =  function(libs) {
    lapply(.libs, library, character.only = TRUE)
  })
  
  parLapply(cl, split(.x, .x$patid %% cores), .f, ...) %>% 
    reduce(funion)
}



# Manipulate antibiotics --------------------------------------------------

regex_bnf <- function(chapter){
  # Get a regular expression that finds BNF chapters among a string of 
  # multiple chapters
  #
  # Args:
  #   chapter - a character string containing the chapter code with the 
  #             desired granularity (e.g. 0501 for systemic antiotics, 
  #             050101 for penicillins)
  #
  # Result:
  #   Character string containing the appropriate regular expression
  
  str_c("^(.{8}/)*", chapter, ".*$")
}






# Captioner package helper funcitons --------------------------------------

chapter <- function(caption, number){
  
  # NOT WORKING YET: aims to set captioner package to current chapter
  
  if(!("package:captioner" %in% search())){
    stop("Package 'captioner' must be loaded before calling chapter()")
  }
  
  cap_obj <- captioner(caption, levels = length(number) + 1)
  
  # Increment to start at given number
  for(n in seq_len(length(number))){
    for(loop in seq_len(number[n])){
      
      if(!(n == length(number) && loop == number[n])){
        cap_obj(str_c("dummy", loop), level = n, display = FALSE) 
      } else {
        cap_obj(str_c("1", loop), level = n, display = FALSE) 
      }
    }
  }
  
  cap_obj
}
