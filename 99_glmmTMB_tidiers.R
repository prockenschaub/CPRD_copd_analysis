options("broom.mixed.sep1" = ",")


#' Tidying methods for glmmTMB models
#'
#' These methods tidy the coefficients of mixed effects models, particularly
#' responses of the \code{merMod} class
#'
#' @param x An object of class \code{merMod}, such as those from \code{lmer},
#' \code{glmer}, or \code{nlmer}
#'
#' @return All tidying methods return a \code{tibble}.
#' The structure depends on the method chosen.
#'
#' @name glmmTMB_tidiers
#'
#' @examples
#' if (require("glmmTMB") && require("lme4")) {
#'     data("sleepstudy",package="lme4")
#'     ## original model:
#'     \dontrun{
#'         lmm1 <- glmmTMB(Reaction ~ Days + (Days | Subject), sleepstudy)
#'     }
#'     ## load stored object
#'     load(system.file("extdata","glmmTMB_example.rda",package="broom.mixed"))
#'     tidy(lmm1)
#'     tidy(lmm1, effects = "fixed")
#'     tidy(lmm1, effects = "fixed", conf.int=TRUE)
#'     tidy(lmm1, effects = "fixed", conf.int=TRUE, conf.method="uniroot")
#'     ## FIX: tidy(lmm1, effects = "ran_vals", conf.int=TRUE)
#'     head(augment(lmm1, sleepstudy))
#'     glance(lmm1)
#'
#'     ## original model:
#'     ##  glmm1 <- glmmTMB(incidence/size ~ period + (1 | herd),
#'     ##                  data = cbpp, family = binomial, weights=size)
#'     tidy(glmm1)
#'     tidy(glmm1, effects = "fixed")
#'     head(augment(glmm1, cbpp))
#'     head(augment(glmm1, cbpp, type.residuals="pearson"))
#'     glance(glmm1)
#' }
NULL


#' @rdname glmmTMB_tidiers
#'
#' @param effects A character vector including one or more of "fixed" (fixed-effect parameters), "ran_pars" (variances and covariances or standard deviations and correlations of random effect terms) or "ran_vals" (conditional modes/BLUPs/latent variable estimates)
#' @param component which component to extract (e.g. \code{cond} for conditional effects (i.e., traditional fixed effects); \code{zi} for zero-inflation model; \code{disp} for dispersion model
#' @param conf.int whether to include a confidence interval
#' @param conf.level confidence level for CI
#' @param conf.method method for computing confidence intervals (see \code{\link[lme4]{confint.merMod}})
#' @param scales scales on which to report the variables: for random effects, the choices are \sQuote{"sdcor"} (standard deviations and correlations: the default if \code{scales} is \code{NULL}) or \sQuote{"varcov"} (variances and covariances). \code{NA} means no transformation, appropriate e.g. for fixed effects; inverse-link transformations (exponentiation
#' or logistic) are not yet implemented, but may be in the future.
#' @param ran_prefix a length-2 character vector specifying the strings to use as prefixes for self- (variance/standard deviation) and cross- (covariance/correlation) random effects terms
#'
#' @return \code{tidy} returns one row for each estimated effect, either
#' with groups depending on the \code{effects} parameter.
#' It contains the columns
#'   \item{group}{the group within which the random effect is being estimated: \code{NA} for fixed effects}
#'   \item{level}{level within group (\code{NA} except for modes)}
#'   \item{term}{term being estimated}
#'   \item{estimate}{estimated coefficient}
#'   \item{std.error}{standard error}
#'   \item{statistic}{t- or Z-statistic (\code{NA} for modes)}
#'   \item{p.value}{P-value computed from t-statistic (may be missing/NA)}
#'
#' @note zero-inflation parameters (including the intercept) are reported
#' on the logit scale
#'
#' @importFrom plyr ldply rbind.fill
#' @import dplyr
#' @importFrom tidyr gather spread
#' @importFrom nlme VarCorr ranef
#' @importFrom stats qnorm confint coef na.omit setNames
## FIXME: is it OK/sensible to import these from (priority='recommended')
## nlme rather than (priority=NA) lme4?
#'
#' @export
tidy.glmmTMB <- function(x, effects = c("ran_pars", "fixed"),
                         component = c("cond", "zi"),
                         scales = NULL, ## c("sdcor",NA),
                         ran_prefix = NULL,
                         conf.int = FALSE,
                         conf.level = 0.95,
                         conf.method = "Wald",
                         ...) {

  ## FIXME:  cleanup
  ##   - avoid (as.)data.frame

  ## R CMD check false positives
  term <- estimate <- .id <- level <- std.error <- . <- NULL

  ss <- stats::coef(summary(x))
  ss <- ss[!sapply(ss, is.null)]
  ## FIXME: warn if !missing(component) and component includes
  ##  NULL terms
  component <- intersect(component, names(ss))
  if (length(component[!component %in% c("cond", "zi")]) > 0L) {
    stop("only works for conditional and (partly for) zero-inflation components")
  }
  ss <- ss[component]
  effect_names <- c("ran_pars", "fixed", "ran_vals")
  if (!is.null(scales)) {
    if (length(scales) != length(effects)) {
      stop(
        "if scales are specified, values (or NA) must be provided ",
        "for each effect"
      )
    }
  }
  if (length(miss <- setdiff(effects, effect_names)) > 0) {
    stop("unknown effect type ", miss)
  }
  ret <- list()
  ret_list <- list()
  if ("fixed" %in% effects) {
    # return tidied fixed effects rather than random
    ret <- lapply(
      ss,
      function(x) {
        x %>%
          as.data.frame(stringsAsFactors = FALSE) %>%
          setNames(c("estimate", "std.error", "statistic", "p.value")) %>%
          tibble::rownames_to_column("term")
      }
    )
    # p-values may or may not be included
    # HACK: use the columns from the conditional component, preserving previous behaviour
    if (conf.int) {
      for (comp in component) {
        cifix <- confint(x,
          method = tolower(conf.method),
          level = conf.level,               
          component = comp,
          estimate = FALSE,
          ## conditional/zi components
          ## include random-effect parameters
          ## as well, don't want those right now ...
          parm = seq(nrow(ret[[comp]])), ...
        ) %>%
          as.data.frame(stringsAsFactors = FALSE) %>%
          setNames(c("conf.low", "conf.high"))
        ret[[comp]] <- bind_cols(
          ret[[comp]],
          cifix
        )
      }
    }
    ret_list$fixed <- bind_rows(ret, .id = "component")
  }
  if ("ran_pars" %in% effects &&
    !all(sapply(VarCorr(x), is.null))) {
    ## FIXME: do something sensible about standard errors, confint

    if (is.null(scales)) {
      rscale <- "sdcor"
    } else {
      rscale <- scales[effects == "ran_pars"]
    }
    if (!rscale %in% c("sdcor", "vcov")) {
      stop(sprintf("unrecognized ran_pars scale %s", sQuote(rscale)))
    }
    ## kluge for now ...
    vv <- list()
    if ("cond" %in% component) {
      vv$cond <- VarCorr(x)[["cond"]]
      class(vv$cond) <- "VarCorr.merMod"
    }
    if ("zi" %in% component) {
      if (!is.null(vv$zi <- VarCorr(x)[["zi"]])) {
        class(vv$zi) <- "VarCorr.merMod"
      }
    }

    ret <- (
      purrr::map(vv, as.data.frame, stringsAsFactors = FALSE)
      %>%
        bind_rows(.id = "component")
        %>%
        mutate_if(., is.factor, as.character)
    )
    if (is.null(ran_prefix)) {
      ran_prefix <- switch(rscale,
        vcov = c("var", "cov"),
        sdcor = c("sd", "cor")
      )
    }

    ## DRY! refactor glmmTMB/lme4 tidiers

    ## don't try to assign as rowname (non-unique anyway),
    ## make it directly into a term column
    if (nrow(ret)>0) {
        ret[["term"]] <- apply(ret[c("var1", "var2")], 1,
                               ran_pars_name,
                               ran_prefix = ran_prefix
                               )

       ## keep only desired term, rename
       ## FIXME: should use select + tidyeval + rename ... ?
       ranpar_names <- c("component", "group", "term", "estimate")
       ret <- setNames(
          ret[c("component", "grp", "term", rscale)],
          ranpar_names
       )
    } else {
        ret <- dplyr::data_frame(component=character(0),
                      group=character(0),
                      term=character(0),
                      estimate=numeric(0))
    }
    ## rownames(ret) <- seq(nrow(ret))

      if (conf.int) {
        thpar <- "theta_"
        if (utils::packageVersion("glmmTMB")<="0.2.2.0") {
             thpar <- which(names(x$obj$par)=="theta")
        }
        ciran <- (confint(x,
                          ## for next glmmTMB (> 0.2.3) can be "theta_",
                          parm = thpar,
                          method = conf.method,
                          level = conf.level,                
                          estimate = FALSE,
                          ...
      )
      %>% as_tibble()
      %>% setNames(c("conf.low", "conf.high"))
      )
      ret <- bind_cols(ret, ciran)
    }
    ret_list$ran_pars <- ret
  }

  if ("ran_vals" %in% effects) {
    ## fix each group to be a tidy data frame

    re <- ranef(x, condVar = TRUE)
    getSE <- function(x) {
      v <- attr(x, "postVar")
      setNames(
        as.data.frame(sqrt(t(apply(v, 3, diag))),
          stringsAsFactors = FALSE
        ),
        colnames(x)
      )
    }
    fix <- function(g, re, .id) {
      newg <- broom::fix_data_frame(g, newnames = colnames(g), newcol = "level")
      # fix_data_frame doesn't create a new column if rownames are numeric,
      # which doesn't suit our purposes
      newg$level <- rownames(g)
      newg$type <- "estimate"

      newg.se <- getSE(re)
      newg.se$level <- rownames(re)
      newg.se$type <- "std.error"

      data.frame(rbind(newg, newg.se),
        .id = .id,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      ## prevent coercion of variable names
    }

    mm <- do.call(rbind, Map(fix, coef(x), re, names(re)))

    ## block false-positive warnings due to NSE
    type <- spread <- est <- NULL
    mm %>%
      gather(term, estimate, -.id, -level, -type) %>%
      spread(type, estimate) -> ret

    ## FIXME: doesn't include uncertainty of population-level estimate

    if (conf.int) {
      if (conf.method != "Wald") {
        stop("only Wald CIs available for conditional modes")
      }

      mult <- qnorm((1 + conf.level) / 2)
      ret <- transform(ret,
        conf.low = estimate - mult * std.error,
        conf.high = estimate + mult * std.error
      )
    }

    ret <- dplyr::rename(ret, grp = .id)
    ret_list$ran_vals <- ret
  }
  ret <- (ret_list
  %>%
    dplyr::bind_rows(.id = "effect")
    %>%
    as_tibble()
    %>%
    reorder_cols()
  )
  return(ret)
}

#' @rdname glmmTMB_tidiers
#'
#' @template augment_NAs
#'
#' @param data original data this was fitted on; if not given this will
#' attempt to be reconstructed
#' @param newdata new data to be used for prediction; optional

#' @return \code{augment} returns one row for each original observation,
#' with columns (each prepended by a .) added. Included are the columns
#'   \item{.fitted}{predicted values}
#'   \item{.resid}{residuals}
#'   \item{.fixed}{predicted values with no random effects}
#'
#' @export
augment.glmmTMB <- function(x, data = stats::model.frame(x), newdata,
                            ...) {
  broom::augment_columns(x, data, newdata, ...)
}

#' @rdname glmmTMB_tidiers
#'
#' @param ... extra arguments (not used)
#'
#' @return \code{glance} returns one row with the columns
#'   \item{sigma}{the square root of the estimated residual variance}
#'   \item{logLik}{the data's log-likelihood under the model}
#'   \item{AIC}{the Akaike Information Criterion}
#'   \item{BIC}{the Bayesian Information Criterion}
#'   \item{deviance}{deviance}
#'
#' @rawNamespace if(getRversion()>='3.3.0') importFrom(stats, sigma) else importFrom(lme4,sigma)
#' @export
glance.glmmTMB <- function(x, ...) {
  finish_glance(x = x)
}
  
  
  
 ## most of these are unexported (small) functions from broom;
## could be removed if these were exported

## https://github.com/klutometis/roxygen/issues/409
#' @importFrom broom tidy glance augment
#' @export
broom::tidy
#' @export
broom::glance
#' @export
broom::augment
#'
#' strip rownames from an object
#'
#' @param x a data frame
unrowname <- function(x) {
  rownames(x) <- NULL
  return(x)
}

## first convert to data frame, then add rownames, then tibble
tibblify <- function(x, var = "term") {
  if (is.null(var)) {
    return(dplyr::as_tibble(unrowname(x)))
  }
  ret <- (x
  %>%
    as.data.frame()
    %>%
    tibble::rownames_to_column(var)
    %>%
    dplyr::as_tibble())
  return(ret)
}

#' Remove NULL items in a vector or list
#'
#' @param x a vector or list
compact <- function(x) Filter(Negate(is.null), x)

#' insert a row of NAs into a data frame wherever another data frame has NAs
#'
#' @param x data frame that has one row for each non-NA row in original
#' @param original data frame with NAs
insert_NAs <- function(x, original) {
  indices <- rep(NA, nrow(original))
  indices[which(stats::complete.cases(original))] <- seq_len(nrow(x))
  x[indices, ]
}

## list of regex matches for mixed-effect columns -> broom names
col_matches <- list(
  estimate = "^(Estimate|Value)$",
  std.error = "Std\\. ?Error",
  df = "df",
  statistic = "(t|Z)[ -]value",
  p.value = "(Pr\\(>|[tZ]\\)|p[ -]value)"
)

## like match(), but with a table of regexes
regex_match <- function(x, table) {
  r <- sapply(
    x,
    function(z) {
      m <- vapply(col_matches, grepl, x = z, ignore.case = TRUE, logical(1))
      if (any(m)) return(which(m)) else return(NA)
    }
  )
  return(unname(r))
}

## rename columns according to regex matches
## names that are not matched are left unchanged
rename_regex_match <- function(x, table = col_matches) {
    rr <- regex_match(names(x), table)
    names(x)[!is.na(rr)] <- names(table)[na.omit(rr)]
    return(x)
}

## convert confint output to a data frame and relabel columns
cifun <- function(x, ...) {
  r <- confint(x, ...) %>%
    data.frame() %>%
    setNames(c("conf.low", "conf.high"))
  return(r)
}


## put specified columns (if they exist) as first columns in output, leave
##  other columns as is
reorder_frame <- function(x, first_cols = c("effect", "group", "term", "estimate")) {
  ## order of first arg to intersect() determines order of results ...
  first_cols <- intersect(first_cols, names(x))
  other_cols <- setdiff(names(x), first_cols)
  return(x[, c(first_cols, other_cols)])
}

## FIXME: store functions to run as a list of expressions,
##  allow user-specified 'skip' argument?
finish_glance <- function(ret = dplyr::data_frame(), x) {
  stopifnot(length(ret) == 0 || nrow(ret) == 1)

  ## catch NULL, numeric(0), error responses

  tfun <- function(e) {
    tt <- tryCatch(eval(substitute(e)), error = function(e) NA)
    if (length(tt) == 0) tt <- NA
    return(tt)
  }

  newvals <- dplyr::data_frame(
    sigma = tfun(sigma(x)),
    logLik = tfun(as.numeric(stats::logLik(x))),
    AIC = tfun(stats::AIC(x)),
    BIC = tfun(stats::BIC(x)),
    deviance = suppressWarnings(tfun(stats::deviance(x))),
    df.residual = tfun(stats::df.residual(x))
  )
  ## drop NA values
  newvals <- newvals[!vapply(newvals, is.na, logical(1))]

  if (length(ret) == 0) {
    return(newvals)
  } else {
    return(dplyr::bind_cols(ret, newvals))
  }
}

######
## experimental finish_glance ...
f2 <- function(ret = data.frame(), x, skip_funs = character(0)) {
  tfun <- function(f) {
    tt <- tryCatch(f(x), error = function(e) NA)
    if (length(tt) == 0) tt <- NA
    return(tt)
  }

  stopifnot(length(ret) == 0 || nrow(ret) == 1)

  funs <- c("logLik", "AIC", "BIC", "deviance", "df.residual")
  funs <- setdiff(funs, skip_funs)

  newvals <- lapply(funs, function(f) as.numeric(tfun(get(f, "package:stats"))))
  newvals <- as.data.frame(newvals)
  names(newvals) <- funs
  ## drop NA values
  newvals <- newvals[!vapply(newvals, is.na, logical(1))]
  if (length(ret) == 0) {
    return(unrowname(newvals))
  } else {
    return(unrowname(data.frame(ret, newvals)))
  }
}

## like process_lm, but without lm-specific confint stuff
## applied *downstream* (after CIs etc have already been added)
trans_coef <- function(ret, x, conf.int = FALSE, conf.level = 0.95, exponentiate = FALSE,
                       trans = identity) {
  ## FIXME: should transform sds as well
  if (missing(trans)) {
    if (exponentiate) {
      if (is.null(x$family) || !grepl("log", x$family$link)) {
        warning(paste(
          "Exponentiating coefficients, ",
          "but model did not use ",
          "a (log, logit, cloglog) link function"
        ))
      }
      trans <- exp
    } else {
      trans <- identity
    }
  }
  ret <- (ret
  %>%
    mutate_at(intersect(c("term", "conf.low", "conf.high")), trans))
  return(ret)
}


## naming function
ran_pars_name <- function(x, ran_prefix) {
  v <- na.omit(unlist(x))
  if (length(v) == 0) v <- "Observation"
  p <- paste(v, collapse = ".")
  if (!identical(ran_prefix, NA)) {
    p <- paste(ran_prefix[length(v)], p,
      sep = getOption("broom.mixed.sep1")
    )
  }
  return(p)
}


## FIXME: 1. sds_..., sigma not properly translated
##        2. names of
## translate brms-style "terms" into standard broom.mixed
## term -> effect, group, term
trans_brms_params <- function(tidy_obj) {
  tt <- tidy_obj[["term"]]
  effcodes <- c("b", "sd", "cor", "s", "sigma", "sds", "r", "lp__")
  neweffcodes <- c(
    "fixed", "ran_pars", "ran_pars",
    "ran_vals", "ran_pars", "???", "ran_vals", "lp__"
  )
  effc2 <- effcodes
  effc2[4] <- "s(?!(igma))" ## negative lookahead ...
  effc2 <- paste0("^(", paste(effc2, collapse = "|"), ")")
  effects <- stringr::str_extract(tt, effc2)
  tt2 <- stringr::str_remove(tt, paste0(effc2, "_?"))
  ## keep r/s distinction a little longer
  ## https://stackoverflow.com/questions/42457189/greedy-regex-for-one-part-non-greedy-for-other?rq=1
  ## (.*?) go until FIRST occurence of next pattern
  ## (?= ...  ) lookahead -- don't include this stuff in the extracted string
  group <- stringr::str_extract(tt2, "(.*?)(?=(__|\\[))")
  grpvals <- effects %in% c("sd", "cor", "r")
  ## remove group__ for sd/cor
  tt2[grpvals] <- stringr::str_remove(tt2[grpvals], "(.*?)__")
  tt2[grpvals] <- stringr::str_remove(tt2[grpvals], "(.*?)(?=(\\[))")
  effects <- as.character(factor(effects,
    levels = effcodes,
    labels = neweffcodes
  ))
  ## replace 'term' (in place) with 'effect', 'group', 'term'
  term_col <- which(names(tidy_obj) == "term")
  prev_cols <- if (term_col > 1) seq(term_col - 1) else numeric(0)
  ## restore sd/cor to beginning of
  res <- bind_cols(tidy_obj[prev_cols],
    effect = effects,
    group = group, term = tt2,
    tidy_obj[(term_col + 1):ncol(tidy_obj)]
  )
  return(res)
}

## enforce consistent column order for *existing* columns
## should contain all possible column names
reorder_cols <- function(x) {
  all_cols <- c(
    "response","effect",
    "component", ## glmmTMB, brms
    "group", "level", "term", "index", "estimate",
    "std.error", "statistic",
    "df", "p.value",
    "conf.low", "conf.high", "rhat", "ess"
  )
  return(select(x, intersect(all_cols, names(x))))
}

rename_cols <- function(x,
                        from = c("Estimate", "Std. Error", "(z|Z|t) value", "Pr\\(>"),
                        to = c("estimate", "std.error", "statistic", "p.value")) {
  if (!is.data.frame(x)) x <- dplyr::as_tibble(x)
  for (i in seq_along(from)) {
    if (length(m <- grep(from[i], names(x))) > 0) {
      names(x)[m] <- to[i]
    }
  }
  return(x)
}