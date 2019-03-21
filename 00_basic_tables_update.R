#--------------------------------------------------------------------------
#
# Program: 01_basic_tables.R
# Author:  Patrick Rockenschaub
# Date:    20/07/2018
#
# Purpose: Create database objects of all the basic tables in the database
#          to access, manipulate and join them from within R
#
# NOTE:    Updated for the complete data extract
#
#--------------------------------------------------------------------------




# Helper tables -----------------------------------------------------------

in_date <- function(sql_tbl, start = "", end = ""){
  # Limit a lazy sql table created with one of the below functions to a 
  # given time period. Start and/or end dates can be used. The complete
  # table can be requested using collect().
  #
  # Args:
  #   sql_tbl - lazy table to limit
  #   start   - follow up start date (use "" to ignore)
  #   end     - follow up end date (use "" to ignore)
  #   
  # Result:
  #   All table records in defined range as a lazy tbl
  
  if("eventdate" %in% op_vars(sql_tbl)){
    # table is a CPRD table
    sql_tbl %>% 
      filter(
        start == "" | eventdate >= start, 
        end == ""   | eventdate <= end
      )
  } else if ("admidate" %in% op_vars(sql_tbl)){
    # table is a HES table
    sql_tbl %>% 
      filter(
        start == "" | discharged >= start , 
        end == ""   | admidate <= end
      )
  } else if ("arrivaldate" %in% op_vars(sql_tbl)){
    # table is a HES A&E table
    sql_tbl %>% 
      filter(
        start == "" | arrivaldate >= start, 
        end == ""   | arrivaldate <= end
      )
  } else if (all(c("start_date", "end_date") %in% op_vars(sql_tbl))){
    # table is a patient table
    sql_tbl %>% 
      mutate(
        # Patient can only enter the cohort once they have been registered
        # for a year
        enter_date = 
          dbplyr::sql(max_db(start, "DATEADD(year, 1, start_date)")),
        leave_date = 
          dbplyr::sql(min_db(end , "end_date"))
      ) %>% 
      filter(enter_date <= leave_date)
    
  } else {
    stop(bl("Table must contain an eventdate/start_date/end_date", 
            "field (CPRD) or admidate/discharged/arrivaldate fields", 
            "(HES)"))
  }
}


first_record <- function(sql_tbl, by = NULL){
  # Obtain the first record of each patient in a database table, as defined 
  # by the eventdate 
  # 
  # Args:
  #   sql_tbl - lazy table from which to determine the first record
  #
  # Result:
  #   All first records as a lazy table
  
  sql_tbl %>%   
    group_by_(.dots = c("patid", by)) %>% 
    filter(eventdate == min(eventdate, na.rm = TRUE)) %>% 
    distinct()
}


last_record <- function(sql_tbl, by = NULL){
  # Obtain the last record of each patient in a database table, as defined 
  # by the eventdate 
  # 
  # Args:
  #   sql_tbl - lazy table from which to determine the first record
  #   by - optional further grouping variables, default none
  #
  # Result:
  #   All last records as a lazy table
  
  sql_tbl %>%   
    group_by_(.dots = c("patid", by)) %>% 
    filter(eventdate == max(eventdate, na.rm = TRUE)) %>% 
    distinct()
}


tbl_exists <- function(name){
  # Determine whether a table exists in the database. Also handles
  # temporary tables prefixed with ##
  #
  # Args:
  #   name - Name of the table (including ## for temp tables)
  #
  # Result:
  #   TRUE if the table exists in the database, FALSE if not
  
  if(length(name) > 1){
    stop("Only supply one table name at a time.")
  }
  
  if(str_detect(name, "^##")){
    # Temporary tables can't be detected by db_has_table()
    error <- try(tbl(get_db_conn(), name), silent = TRUE)
    return(class(error)[1] != "try-error")
  }
  
  db_has_table(get_db_conn(), name)
}


remove_temporary <- function(names){
  # Remove tables if the table currently exists in the database
  #
  # Args:
  #   names - a character vector of table names starting with "##"
  #
  # Result:
  #   NULL
  
  if(!all(str_detect(names, "^##"))){
    stop("Only provide names suffixed with ## (i.e. no permanent tables")
  }
  
  exist <- map_lgl(names, tbl_exists)
  walk(names[exist], dbRemoveTable, conn = get_db_conn())
}




# Lookup tables -----------------------------------------------------------

medical <- function(){
  # Select the complete Read code dictionary in the database. The complete 
  # table can be requested using collect().
  #
  # Args:
  #
  # Result:
  #   The complete Read code dictionary as a lazy tbl
  
  check_connection()
  
  tbl(get_db_conn(), "lu_medical")
}


product <- function(){
  # Select a copy of the product lookup table in the database. The complete 
  # table can be requested using collect().
  #
  # Args:
  #   
  # Result:
  #   Product information as a lazy tbl
  
  check_connection()
  
  tbl(get_db_conn(), "lu_product")
}


product_name <- function(){
  # Select only the columns prodcode and productname from the product 
  # lookup table in the database. The complete table can be requested using 
  # collect().
  #
  # Args:
  #   
  # Result:
  #   Limited product information as a lazy tbl
  
  product() %>% 
    dplyr::select(prodcode, productname)
}


product_bnf <- function(){
  # Select only the columns prodcode and bnfcode from the product 
  # lookup table in the database. The complete table can be requested using 
  # collect().
  #
  # Args:
  #   
  # Result:
  #   Limited product information as a lazy tbl
  
  product() %>% 
    dplyr::select(prodcode, bnfcode)
}


antibiotics <- function(){
  # Select the lookup table for antibiotic definitions and classifications.
  # the complete table can be requested using collect().
  #
  # Args:
  #   
  # Result:
  #   Antibiotic classifications as a lazy tbl
  
  check_connection()
  
  tbl(get_db_conn(), "codes_abx")
}




# Raw tables --------------------------------------------------------------

practice_db <- function(){
  # Select the additional practice information in the database. The 
  # complete table can be requested using collect().
  #
  # Args:
  #   
  # Result:
  #   The basic practice information in the database
  
  check_connection()
  
  all_patients_db() %>%
    rename(uts = prac_uts, lcd = prac_lcd, region = prac_region) %>% 
    group_by(pracid, uts, lcd, region) %>% 
    summarise(linked = max(hes15, na.rm = TRUE)) %>% # has linkage if one is linked
    ungroup()
}


consult_db <- function(){
  # Select all consultation information in the database. The 
  # complete table can be requested using collect().
  #
  # Args:
  #   
  # Result:
  #   The raw consultation information in the database (BIG!)
  
  check_connection()
  
  tbl(get_db_conn(), "consultation")
}


clinical_db <- function(){
  # Select all clinical information in the database. The 
  # complete table can be requested using collect().
  #
  # Args:
  #   
  # Result:
  #   The raw clinical information in the database (BIG!)
  
  check_connection()
  
  tbl(get_db_conn(), "clinical")
}



test_db <- function(){
  # Select all diagnostic test information in the database.  
  # The complete table can be requested using collect().
  #
  # Args:
  #   
  # Result:
  #   The raw test information in the database (BIG!)
  
  check_connection()
  
  tbl(get_db_conn(), "test")
}


immun_db <- function(){
  # Select all immunisation information in the database.  
  # The complete table can be requested using collect().
  #
  # Args:
  #   
  # Result:
  #   The raw immunisation information in the database (BIG!)
  
  check_connection()
  
  tbl(get_db_conn(), "immunisation")
}


referral_db <- function(){
  # Select all referral information in the database.  
  # The complete table can be requested using collect().
  #
  # Args:
  #   
  # Result:
  #   The raw immunisation information in the database (BIG!)
  
  check_connection()
  
  tbl(get_db_conn(), "referral")
}



therapy_db <- function(){
  # Select all therapy information in the database. The 
  # complete table can be requested using collect().
  #
  # Args:
  #   
  # Result:
  #   The raw therapy information in the database (BIG!)
  
  check_connection()
  
  tbl(get_db_conn(), "therapy")
}



# Code lists --------------------------------------------------------------

codes_abx_db <- function(){
  # Get all types of antibiotics in the database.
  # 
  # Args:
  #      
  #      
  # Result:    
  #   A lazy table with all antibiotic definitions in the database
  
  
  check_connection()
  
  tbl(get_db_conn(), "codes_abx")
}

codes_qof_db <- function(){
  # Get all QOF Read codes in the database.
  # 
  # Args:
  #      
  #      
  # Result:    
  #   A lazy table with all QOF Read codes in the database
  
  
  check_connection()
  
  tbl(get_db_conn(), "codes_qof")
}


codes_obese_db <- function(){
  # Get all obesity related Read codes in the database.
  # 
  # Args:
  #      
  #      
  # Result:    
  #   A lazy table with all obesity codes in the database
  
  
  check_connection()
  
  tbl(get_db_conn(), "codes_obese")
}


codes_smoke_db <- function(){
  # Get all smoking related Read codes in the database.
  # 
  # Args:
  #      
  #      
  # Result:    
  #   A lazy table with all smoke codes in the database
  
  
  check_connection()
  
  tbl(get_db_conn(), "codes_smoke")
}



# Views to speed up computation -------------------------------------------

qof_db <- function(){
  # Select the view in the database that contains all the QOF codes in the
  # clinical table and the test table
  #
  # Args:
  #   
  #
  # Result:
  #   The entire view in the database.
  
  check_connection()
  
  tbl(get_db_conn(), "vw_qof")
}



infect_db <- function(){
  # Select the view in the database that contains all the infection codes in 
  # the clinical table and the test table
  #
  # Args:
  #   
  #
  # Result:
  #   The entire view in the database.
  
  check_connection()
  
  tbl(get_db_conn(), "vw_infect")
}


indication_db <- function(){
  # Select the view in the database that contains all the infection codes in 
  # the clinical table and the test table
  #
  # Args:
  #   
  #
  # Result:
  #   The entire view in the database.
  
  check_connection()
  
  dplyr::union(
    tbl(get_db_conn(), "vw_same_day_or_cons_clinical"),
    tbl(get_db_conn(), "vw_same_day_or_cons_test")
  )
}


abx_db <- function(){
  # Select the view in the database that contains all the antibiotic codes 
  # in the therapy table
  #
  # Args:
  #   
  #
  # Result:
  #   The entire view in the database.
  
  check_connection()
  
  tbl(get_db_conn(), "vw_abx")
}



obese_db <- function(){
  # Select the helper table in the database that contains all obesity
  # records from the clinical table
  #
  # Args:
  #   
  #
  # Result:
  #   The entire table in the database.
  
  check_connection()
  
  tbl(get_db_conn(), "vw_clinical_obese_codes") %>% 
    select(-num_rows)
}


bmi_db <- function(){
  # Select the views in the table that contain height and weight
  # and combine them. Needs to be post-processed in R, as height and
  # weight must be carried forward if they weren't measured on the 
  # same day.
  #
  # Args:
  #   
  #
  # Result:
  #   The combined table in the database.
  
  check_connection()
  
  weight <- 
    tbl(get_db_conn(), "vw_clinical_weight") %>% 
    select(patid:staffid, weight = data1, bmi = data3)
    
  height <- 
    tbl(get_db_conn(), "vw_clinical_height") %>% 
    select(patid, eventdate, height = data1)
  
  full_join(weight, height, by = c("patid", "eventdate"))
}


smoke_db <- function(){
  # Select the helper table in the database that contains all smoke status
  # records from the clinical table
  #
  # Args:
  #   
  #
  # Result:
  #   The entire table in the database.
  
  check_connection()
  
  tbl(get_db_conn(), "vw_clinical_smoke")
}





# Cohorts -----------------------------------------------------------------

all_patients_db <- function(){
  # Select all patients in the database. This includes non-HES linked 
  # patients, who were not part of the original study population. Use for 
  # sensitivity analysis only. The complete table can be requested using 
  # collect().
  #
  # Args:
  #   
  # Result:
  #   The basic cohort definition in the database

  check_connection()
  
  tbl(get_db_conn(), "cohort") %>% 
    mutate(
      birth_date = dplyr::sql("DATEFROMPARTS(YEAR(dob), 7, 1)"),
      female = gender - 1
    ) %>% 
    rename(start_date = data_start, end_date = data_end)
}


cohort_full_db <- function(){
  # Select the main study population in the database. The complete table
  # can be requested using collect().
  #
  # Args:
  #   
  # Result:
  #   The basic cohort definition in the database
  
  all_patients_db() %>% 
    filter(hes15 == 1, death15 == 1, lsoa15 == 1) %>% 
    left_join(death_db(), by = c("patid", "pracid")) %>% 
    select(-gen_death_id, -dod_partial) %>% 
    mutate (# Make sure to set end date to death date confirmed by ONS
      end_date = dplyr::sql("IIF(dod IS NOT NULL and dod < end_date, dod, end_date)")
    ) %>% 
    rename(death_date = dod)
}


study_population_db <- function(link = TRUE){
  # Select the main study population in the database (either HES or all). 
  # The complete table can be requested using collect().
  #
  # NOTE: ONS death date is used to ascertain death date in linked cohort.
  #
  # Args:
  #   link - if true, only linked patients are selected
  #   
  # Result:
  #   The basic cohort definition in the database
  
  cols <- c("patid", "pracid", "female", "birth_date", 
            "start_date", "end_date")
  
  if(link){
    cols <- c(cols, "death_date")
    
    tab <- cohort_full_db()
  } else {
    cols <- c(cols, "hes15", "death15", "lsoa15")
    
    tab <- all_patients_db()
  }
  
  tab %>% dplyr::select_(.dots = cols)
}




# Antibiotics -------------------------------------------------------------


filter_bnf <- function(sql_tbl, bnf, include = TRUE){
  # Helper function to filter the BNF chapters in the database.
  #
  # Args:
  #   sql_tbl - a table object still hold in the database
  #   bnf     - a single character string containing the bnf chapter
  #   include - a logical value indicating whether the chapter should
  #             be kept or removed
  #
  # Returns:
  #   a tbl_sql object with the filter applied
  
  if(!inherits(sql_tbl, "tbl_sql")){
    stop("Parameter sql_tbl must be of class tbl_sql.")
  }
  
  pos <- (0:9) * 9 + 1
  
  if(include){
    filter(sql_tbl, CHARINDEX(bnf, bnfcode) %in% pos)
  } else {
    filter(sql_tbl, !(CHARINDEX(bnf, bnfcode) %in% pos))
  }
}



abx_bnf_db <- function(include, exclude = NULL){
  # Select all antibiotic prescriptions in the database that match one
  # inclusion chapter and potentially multiple exclusion chapters.
  #
  # Args:
  #   include - a BNF chapter string to be kept
  #   exclude - one or more subchapters to exclude. Leave as NULL if 
  #             no chapter should be excluded
  #
  # Returns:
  #   a tbl_sql object with the filter applied
  
  res <- 
    abx_db() %>%
    inner_join(product_bnf(), by = "prodcode") %>% 
    filter_bnf(include)
  
  for(code in exclude){
    res <- filter_bnf(res, code, include = FALSE)
  }
  
  res
}




# Diagnoses -----------------------------------------------------------

records_db <- function(sql_tbl, code_dt, id = "medcode"){
  # Select records in the database based on some form of codelist. The 
  # complete table can be requested using collect().
  #
  # Args:
  #   sql_tbl - a object of tbl_sql in which to search for the codes
  #   code_dt - a tbl_sql or data.table with a relevant id column
  #   
  # Result:
  #   All records as a lazy query
  
  check_connection()
  
  
  if("tbl_sql" %in% class(code_dt)){
    # If the codes are already in the database
    codes <- code_dt
    
  } else if ("data.frame" %in% class(code_dt)){
    # If the codes are in a data.frame
    tmp <- digest::sha1(code_dt)
    
    # If the table has not been computed previously, compute now
    if(!tbl_exists(paste0("##", tmp))){
      codes <- copy_to(get_db_conn(), code_dt, tmp)
    } else {
      codes <- tbl(get_db_conn(), paste0("##", tmp))
    }
  } else {
    stop("Only tbl_sql or data.frame objects allowed for code_dt")
  }
  
  semi_join(sql_tbl, codes, by = id)
}



# Hospital ----------------------------------------------------------------

# NOTE: this section is still using old tables in the database

admimeth <- function(){
  # Table that contains the mapping to admission methods lookup table
  #
  # Args:
  #   
  # Result:
  # 
  
  check_connection()
  
  tbl(get_db_conn(), "admimeth")
}


admissions_db <- function(){
  # Select all hospital admission information from HES. 
  #
  # Args:
  #   
  # Result:
  #   All hospital admission information as a lazy tbl
  
  check_connection()
  
  tbl(get_db_conn(), "inpat_admission") %>% 
    inner_join(admimeth(), by = "admimeth") %>% 
    dplyr::select(-admimeth, -admidesc)
}


emergency_db <- function(){
  # Select all hospital admissions from HES that are marked as emergency
  # admissions.
  #
  # Args:
  #   
  # Result:
  #   All emergency hospital admission information as a lazy tbl
  
  admissions_db() %>% 
    filter(admitype == "Emergency")
}


elective_db <- function(){
  # Select all hospital admissions from HES that are marked as elective
  # admissions.
  #
  # Args:
  #   
  # Result:
  #   All elective hospital admission information as a lazy tbl
  
  admissions_db() %>% 
    filter(admitype == "Elective")
}


hosp_diag_db <- function(){
  # Select all hospital diagnoses (only infections) from HES.
  #
  # Args:
  #   
  # Result:
  #   All hospital infection diagnosis information
  
  check_connection()
  
  tbl(get_db_conn(), "inpat_diagnosis")
}


ae_db <- function(){
  # Select all A&E attendances from HES. 
  #
  # Args:
  #   
  # Result:
  #   All hospital attendances as a lazy tbl
  
  check_connection()
  
  tbl(get_db_conn(), "ae_attendance") %>% 
    select(-aekey, -ethnic)
}





# Others ------------------------------------------------------------------

imd_db <- function(){
  # Select all records for the Index of Multiple Deprivation in the 
  # database. The complete table can be requested using collect().
  #
  # Args:
  #
  # Result:
  #   All IMD records as a lazy tbl
  
  tbl(get_db_conn(), "imd")
}


death_db <- function(){
  # Select all death records in the database. The complete table can be 
  # requested using collect().
  #
  # Args:
  #
  # Result:
  #   All IMD records as a lazy tbl
  
  check_connection()
  
  tbl(get_db_conn(), "death")
}


