###########################################################################
# Author:   Patrick Rockenschaub
# Project:  Preserve Antibiotics through Safe Stewardship (PASS)
#           Primary Care work package 1
#           COPD analysis
#
# File:     00_tabulation.R
# Date:     07/11/2018
# Task:     Functions that allow tabeling variables and outputting them as
#           kable
#
###########################################################################



# Main tabulation functions -----------------------------------------------

tab <- function(def, fun = tab_default, ...){
  # Main function to describe the tabulation of a single variable, 
  # potentially stratified by other variables.
  #
  # Args:
  #   def - formula, with the tabulated variable on the left-hand side and
  #         all other variables on the right side: num_abx ~ age + sex
  #   fun - the summary function to be used, e.g. mean or median. Must be
  #         a function that takes a data.table, a variable name and a 
  #         vector of names of grouping variables. See below. 
  #   ... - other parameters passed to .tab()
  #
  # Result:
  #   function that can be given a data.table and performs the tabulation
  
  def <- .f_as_table_def(def)
  args <- c(def, fun = fun, ...)
  
  do.call(partial, args = c(...f = .tab, args))
}


.tab <- function(dt, var, group, fun, fun.group = .N, aux = NULL, cast = "value",
                 show_miss = TRUE, col = TRUE, keep_only = FALSE){
  # Main function that performs the tabulation given a data.table, variable to 
  # tabulate, grouping variables, a summery function and additional parameters.
  # This function is partially computed and returned by tab().
  #
  # Args:
  #   dt - data.table containing the variables to tabulate
  #   var - character name of the variable to tabulate
  #   group - character vector with the names of the variables to group by
  #   fun - the summary function to be used, e.g. mean or median. Must be
  #         a function that takes a data.table, a variable name and a 
  #         vector of names of grouping variables. See below. 
  #   fun.group - expression used to calculate the size of each subgroup
  #   aux - any auxilliary varibles that should be kept internally while 
  #         tabulating to allow the calculation of certain quantities (e.g. 
  #         time at risk for tabulation of rates). Will not appear in the output
  #   cast - name of the variables that will be cast into wide format
  #   show_miss - flag to indicate whether missing values should be shown as 
  #               a row in the final output
  #   col - flag to compute column percent
  #   keep_only - character vector of variable levels to keep. FALSE to keep
  #               all levels
  #
  # Result:
  #   data.table with the calculated table
  
  
  fun.group <- substitute(fun.group)
  
  key_var <- key(dt)
  dt %<>% .[, c(key_var, var, group, aux), with = FALSE] %>% copy()
  
  if(is.null(group)){
    dt[, .all := "all"]
    group <- ".all"
  }
  
  dt[!is.na(get(var)), n_group := eval(fun.group), by = c(if(col) group else var)]
  
  desc <- fun(dt[!is.na(get(var))], var, group)
  desc %<>% dcast(as.formula(str_c("var ~ ", str_c(group, collapse = "+"))), 
                  value.var = cast)
  
  if(show_miss != FALSE && any(is.na(dt[[var]]))){
    miss_fun <- function(x, n_group) { prty(sum(is.na(x))) }
    
    if(show_miss == "%"){
      miss_fun <- function(x, n_group) { prty(`%`(sum(is.na(x)), n_group)) }
    } else if(show_miss == "n (%)"){
      miss_fun <- function(x, n_group) { `n_%`(sum(is.na(x)), n_group) }
    }
    
    miss <- dt[, .(var = "Missing", N = miss_fun(get(var), .N)), by = group]
    
    miss %<>% dcast(as.formula(str_c("var ~ ", str_c(group, collapse = "+"))), 
                    value.var = "N")
    desc %<>% rbind(miss)
  }
  
  desc[is.logical(keep_only) | var %in% keep_only, map(.SD, as.character)]
}




# Helper functions --------------------------------------------------------


.f_as_table_def <- function(f) {
  # Helper function that turns a formula in a table definition as needed by
  # tab()
  #
  # Args:
  #   f - formula
  #
  # Result:
  #   list with left formula side as character named var and right side
  #   as character vector named group
  
  lhs <- function(x) if (length(x) == 2) x else x[-3]
  rhs <- function(x) if (length(x) == 2) NULL else x[-2]
  var <- all.vars(lhs(f))
  group <- all.vars(rhs(f))
  
  if(length(var) > 1) stop("Only one dependent variable accepted")
  if(length(group) == 0 | "." %in% group) group = NULL
  
  list(var = var, group = group)
}

.f_replace <- function(f, with) {
  # Helper function that replaces the right side of a formula with a new
  # formula. 
  #
  # Args:
  #   f - formula (one or two-sided)
  #   with - new right side formula (one-sided)
  #
  # Result:
  #   formula in which the right side of f is replaced with the right 
  #   side of with
  
  f[length(f)] <- with[length(with)]
  f
}


annotate_table <- function(rendered, sections){
  # Helper function that takes a kable table and a list of variable definitions
  # and groups rows according to sublists in the variable definition (used to 
  # create subsections in the table that tabulates similar variables).
  #
  # Args:
  #   rendered - a kable object
  #   sections - a list of table definitions
  #
  # Result:
  #   a kable object with grouped rows
  
  i <- 1L
  for(v in names(sections)){
    n_rows <- nrow(sections[[v]])
    
    if(n_rows > 1) rendered %<>% group_rows(v, i, i + n_rows - 1)
    
    i <- i + n_rows
  }
  rendered
}


render_tab <- function(def, dt, combine = FALSE){
  # Helper function that tabulates a list of variable definitions 
  #
  # Args:
  #   def - a named list of tab() functions. Can be nested.
  #   dt - a single data.table or a list of data.tables the same 
  #        length as length(def)
  #   combine - if FALSE, return a list of tables, otherwise combine
  #             them into a single data.table
  #
  # Result:
  #   data.table or list of data.tables with the tabulated variables
  
  if("data.frame" %in% class(dt)){
    dt <- list(dt)
  }
  
  rendered <- map2(def, dt, ~ if(is.list(.x)) { 
    render_tab(def = .x, .y, combine = TRUE) 
  } else { 
    do.call(.x, args = list(dt = .y)) 
  })
  
  walk2(rendered, names(rendered), ~ if(nrow(.x) == 1) { .x[, var := .y] })
  
  if(combine) rendered <- rbindlist(rendered)
  
  rendered
}



# Summary functions -------------------------------------------------------

mean_sd <- function(x, digits = c(2, 1), na.rm = TRUE){
  # Calculate the mean and standard deviation of a numeric vector
  #
  # Args:
  #   x - numeric vector
  #   digits - number of digits for the mean [1] and the sd [2]
  #   na.rm - should missing values be removed before calculation?
  #
  # Result:
  #   the mean and standard deviation formated as character
  
  str_c(prty(mean(x, na.rm = na.rm), digits[1]),
        " (", prty(sd(x, na.rm = na.rm), digits[2]), ")")
}

median_iqr <- function(x, digits = c(2, 1), na.rm = TRUE){
  # Calculate the median and interquartile range of a numeric vector
  #
  # Args:
  #   x - numeric vector
  #   digits - number of digits for the mean [1] and the sd [2]
  #   na.rm - should missing values be removed before calculation?
  #
  # Result:
  #   the median and interquartile range formated as character
  
  str_c(prty(median(x, na.rm = na.rm), digits[1]),
        " (", str_c(prty(quantile(x, c(.25, .75), na.rm = na.rm), digits[2]), collapse = "-"), ")")
}




# Tabulation functions used by tab() --------------------------------------

tab_default <- function(dt, var, group){
  # Define how categorical and continuous variables sould be
  # summarised for the tables
  #
  # Args:
  #   dt - data.table with the source data
  #   var - name of the column to be summarised
  #   group - names of the grouping variables
  #
  # Result:
  #   summarised column
  
  x <- dt[[var]]
  
  if(is.list(x)){
    x <- unlist(x)
  }
  
  if(is.character(x) || is.factor(x)){
    sum_fun <- tab_n_perc
  } else if(is.numeric(x)){
    sum_fun <- tab_mean_sd
  } else {
    stop("Variables should either be categorical or numeric.")
  }
  
  sum_fun(dt, var, group)
}


tab_n <- function(dt, var, group){
  # Tabulate a column by the number of rows of each variable level
  #
  # Args:
  #   see `tab_default()`
  #
  # Result:
  #   summarised column
  
  
  dt <- dt[, .(value = prty(.N)), by = c(group, var)]
  setnames(dt, var, "var")
  dt
}


tab_perc <- function(dt, var, group){
  # Tabulate a column by the number of rows of each variable level
  #
  # Args:
  #   see `tab_default()`
  #
  # Result:
  #   summarised column
  
  
  dt <- dt[, .(value = prty(`%`(.N, unique(n_group)), 1)), by = c(group, var)]
  setnames(dt, var, "var")
  dt
}


tab_n_perc <- function(dt, var, group){
  # Tabulate a column by the number of rows of each variable level
  # and their relative percentage within the group (column percent)
  #
  # Args:
  #   see `tab_default()`
  #
  # Result:
  #   summarised column
  
  dt <- dt[, .(value = `n_%`(.N, unique(n_group))), by = c(group, var)]
  setnames(dt, var, "var")
  dt
}


tab_n_N_perc <- function(dt, var, group){
  # Tabulate the number of rows of each variable level
  # and their relative percentage within the group (column percent)
  #
  # Args:
  #   see `tab_default()`
  #
  # Result:
  #   summarised column
  
  dt <- dt[, .(value = str_c(.N, "/", unique(n_group), 
                             " (", prty(`%`(.N, unique(n_group)), 1), ")")), 
               by = c(group, var)]
  setnames(dt, var, "var")
  dt
}


tab_mean_sd <- function(dt, var, group){
  # Tabulate the mean and standard deviation of a variable
  # within each group
  #
  # Args:
  #   see `tab_default()` and `mean_sd()``
  #
  # Result:
  #   summarised column 
  
  dt[, .(var = "mean (sd)", value = mean_sd(get(var))), by = group]
}


tab_median_iqr <- function(dt, var, group){
  # Tabulate the median and interquartile range of a variable
  # within each group
  #
  # Args:
  #   see `tab_default()` and `median_iqr()``
  #
  # Result:
  #   summarised column 
  
  dt[, .(var = "median (iqr)", value = median_iqr(get(var))), by = group]
}


