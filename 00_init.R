###########################################################################
# Author:   Patrick Rockenschaub
# Project:  Preserve Antibiotics through Safe Stewardship (PASS)
#           Primary Care work package 1
#           COPD analysis
#
# File:     00_init.R
# Date:     07/11/2018
# Task:     Initialise the workspace for a clean new analysis of antibiotic
#           prescribing in COPD patients (i.e. remove previous objects, 
#           load libraries, etc.)
#
###########################################################################


# Load the general environment (only if not called before)
if(exists(".conn")){
   disconnect_db()
}

# Load the base settings and functions
suppressMessages({
  source("00_init.R")
  source("00_basic_tables_update.R")
  source("00_code_lists.R")
})

# Load local functions
subfolder <- "04_copd"
source(file.path(subfolder, "00_functions.R"))

# Set path to store the derived datasets
der_dir <- "01_derived"

# Set default values
comorbidity <- "copd"

study_start <- ymd("2008-01-01") # start of overall follow-up
main_start <- ymd("2015-01-01")  # start of main analysis
study_end <- ymd("2015-12-31")

lookback <- years(2)