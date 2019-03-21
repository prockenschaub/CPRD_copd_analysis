




# General functions -------------------------------------------------------

char <- function(...){
  # Convert non-standard evalutation names (e.g. column names) to a 
  # character vector.
  # 
  # Args:
  #   ... - a sequence of non-standard evaluation names
  #
  # Result:
  #   a character vector
  #
  # Example:
  #   char(a, b, c) --> c("a", "b", "c")
  
  as.vector(map_chr(rlang::quos(...), rlang::quo_name))
}


require_tbls <- function(tbl_names){
  # Look if all tables (defined as lists) exist in this or a parent
  # parent environment. Throw an error if not.
  #
  # Args:
  #   tbl_names - a character vector of table names to look for
  #
  # Result:
  #   NULL (or error if one of the tables does not exist)
  
  if(any(!map_lgl(tbl_names, exists, mode = "list"))){
    stop(str_c("The following tables are required and missing:\n\n", 
               str_c(tbl_names, collapse = ", ")))
  }
}




# Load and save derived tables --------------------------------------------

save_derived <- function(x, filename, compress = "none"){
  # Save a file into the locally defined derived folder (wrapper around
  # `write_rds()` of the readr package)
  #
  # Args:
  #   x - data.table
  #   filename - the name of the rds file (without the extension '.rds')
  #
  # Result:
  #   NULL
  
  path <- file.path(subfolder, der_dir, str_c(filename, ".rds"))
  write_rds(x, path, compress)
}


load_derived <- function(filename, object_name = filename){
  # Read a file from the locally defined derived folder (wrapper around
  # `read_rds()` of the readr package)
  #
  # Args:
  #   filename - the name of the rds file (without the extension '.rds')
  #
  # Result:
  #   the stored object (invisble)
  
  obj <- read_rds(file.path(subfolder, der_dir, str_c(filename, ".rds")))
  assign(object_name, obj, envir = globalenv())
  
  invisible(obj)
}




# Functions to find records of a certain order ----------------------------

first_record <- function(dt){
  # Get the earliest record for each patient in a data.table
  #
  # Args:
  #   dt - data.table with patient id and eventdate
  #
  # Result:
  #   a data.table with one row per patient, which is the earliest event
  
  
  dt[order(patid, eventdate), .SD[1], by = patid]
}


closest_in <- function(dt, range){
  # Find the closest (i.e. latest) record in the time range given
  #
  # Args:
  #   dt - data.table with an eventdate column
  #   range - a list of two dates (start and end)
  #
  # Result:
  #   subsetted dataset with one row per patient with a record in range
  
  dt[eventdate %between% range, .SD[.N], by = patid][, !("eventdate")]
}



# Functions to deal with missing observations -----------------------------

fill_na <- function(dt, cols = names(dt), with){
  # Fill in missing values in columns of a data.table by reference
  #
  # Args:
  #   dt - data.table in which to replace the missing values
  #   cols - the names of the columns where NAs whould be replaced:
  #          default all columns
  #   with - the value with which to replace the columns
  
  if(length(with) > 1 && length(with) != length(cols)){
    stop("`with` must either be of length 1 or the same length as `cols`")
  }
  
  if(length(with) == 1){
    with <- rep(with, length(cols))
  } 
  
  names(with) <- cols
  
  for(j in cols){
    set(dt, which(is.na(dt[[j]])), j, with[[j]])
  }
}




# Functions to summarise models -------------------------------------------

norm_ci <- function(mle, sd, type = c("response", "linpred"), alpha = 0.05){
  # Calculate an approximate normal confidence interval from a maximum
  # likelihood and a standard deviation vector. CI are available on the 
  # response scale (i.e. exponentiated) and on the scale of the linear 
  # predictor
  
  type <- match.arg(type)
  
  ci <- 
    data.table(
      lower = mle + qnorm(alpha / 2) * sd,
      upper = mle + qnorm(1 - alpha / 2) * sd
    )
  
  if(type == "response"){
    return(ci[, map(.SD, exp)])
  }
  
  ci
}


crude <- function(mod, var, zi = FALSE){
  # Extract crude fixed effects from a fitted log-link model and 
  # exponentiate them. Ignores interaction effects
  #
  # Args:
  #   mod - fitted glm or glmmTMB log-link model
  #   var - name of the variable for which to extract effects
  #   zi - get coefficients from zi part of model (only for glmmTMB)
  #
  # Result:
  #   data.table with variable names, levels, effect and 95%-CI
  
  if(class(mod)[1] == "glmmTMB"){
    x <- mod$frame[[var]]
    if(zi){
      coefs <- summary(mod)$coefficients$zi 
      coefs[, "Estimate"] <- -coefs[, "Estimate"] # negate to get prob of getting something
    } else {
      coefs <- summary(mod)$coefficients$cond
    }
  } else {
    x <- mod$data[[var]]
    coefs <- summary(mod)$coefficients
  }
  
  if(is.factor(x)){
    lvls <- levels(x)
    ref <- 
      data.table(variable = var, value = lvls[1], effect = 1, lower = NA, upper = NA)
  } else {
    ref <- NULL
  }
  
  eff <- 
    coefs %>% 
    as.data.table(keep.rownames = TRUE) %>% 
    .[
      str_detect(rn, var) & !str_detect(rn, ":"), c(
        .(variable = var, value = str_replace(rn, var, ""), effect = exp(Estimate)), 
        norm_ci(Estimate, `Std. Error`)
      )]
  
  rbind(ref, eff)
}

rate <- function(mod, var, mult = 365){
  # Extract the mean rates from a fitted poisson model and 
  # exponentiate them. Ignores interaction effects, but adjusts
  # for overdispersion in case of a multilevel model
  #
  # Args:
  #   mod - fitted glm or glmmTMB Poisson model
  #   var - name of the variable for which to extract effects
  #
  # Result:
  #   data.table with variable names, levels, effect and 95%-CI
  
  disp <- 0L
  
  if(class(mod)[1] == "glmmTMB"){
    x <- mod$frame[[var]]
    coefs <- summary(mod)$coefficients$cond
    
    if(summary(mod)$family == "poisson") {
      # Get the observation level variance if poisson
      disp <- as.numeric(summary(mod)$varcor$cond$patid)
    }
  } else {
    x <- mod$data[[var]]
    coefs <- summary(mod)$coefficients
  }
  
  if(!is.factor(x)){
    warning("This function only works for factors")
  } 
  
  lvls <- levels(x)
  
  rate <- 
    as.data.table(coefs, keep.rownames = TRUE) %>% 
    .[str_detect(rn, str_c("Intercept|", var)) & !str_detect(rn, ":")]
  
  rate$Estimate[-1] <- rate$Estimate[-1] + rate$Estimate[1] # add the intercept
  rate$Estimate <- rate$Estimate + disp / 2 # convert log-normal median into mean
  
  rate <- rate[, c(.(variable = var, value = lvls, effect = exp(Estimate)), 
                   norm_ci(Estimate, `Std. Error`))]
  
  rate[, c("effect", "lower", "upper") := map(.SD, ~ . * mult), .SDcols = effect:upper]
  rate[]
}





# Functions enhancing kableExtra ------------------------------------------

lb <- function(x, format, ...){
  # Add the correct linebreak depending on the Rmarkdown format. Linebreaks
  # in x must be marked by '\\n', e.g. "first line\nsecond line"
  #
  # Args:
  #   x - character vector of length 1
  #   format - "html" or "latex"
  #
  # Result:
  #   formatted character
  
  if(format == "html"){
    str_replace_all(x, "\\\\\\\\n", "<br />")
  } else if(format == "latex"){
    linebreak(x, ...)
  } else {
    stop("only HTML and LaTex formats allowed.")
  }
}
