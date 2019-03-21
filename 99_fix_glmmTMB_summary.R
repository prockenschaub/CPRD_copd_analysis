#--------------------------------------------------------------------------
#
# Program: 99_fix_glmmTMB_summary.R
# Author:  Patrick Rockenschaub
# Date:    17/07/2018
#
# Purpose: glmmTMB throws an error when calling summary on a zero-inflated
#          model with different random effect structures for the count and
#          the zero inflation part. This issue has been flagged with the 
#          package maintainers, but the below functions provid a quick fix
#          until it is updated.
#
#--------------------------------------------------------------------------


# summary.glmmTMB must be overwritten, as it otherwise does not use the  
# custom VarCorr.glmmTMB further down.
summary.glmmTMB <- function (object, ...) 
{
  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }
  sig <- sigma(object)
  famL <- family(object)
  mkCoeftab <- function(coefs, vcov) {
    p <- length(coefs)
    coefs <- cbind(Estimate = coefs, `Std. Error` = sqrt(diag(vcov)))
    if (p > 0) {
      coefs <- cbind(coefs, (cf3 <- coefs[, 1]/coefs[, 
                                                     2]), deparse.level = 0)
      statType <- "z"
      coefs <- cbind(coefs, 2 * pnorm(abs(cf3), lower.tail = FALSE))
      colnames(coefs)[3:4] <- c(paste(statType, "value"), 
                                paste0("Pr(>|", statType, "|)"))
    }
    coefs
  }
  ff <- fixef(object)
  vv <- vcov(object)
  coefs <- setNames(lapply(names(ff), function(nm) if (glmmTMB:::trivialFixef(names(ff[[nm]]), 
                                                                    nm)) 
    NULL
    else mkCoeftab(ff[[nm]], vv[[nm]])), names(ff))
  llAIC <- glmmTMB:::llikAIC(object)
  varcor <- VarCorr(object)
  structure(list(logLik = llAIC[["logLik"]], family = famL$fami, 
                 link = famL$link, ngrps = glmmTMB:::ngrps.glmmTMB(object), nobs = nobs(object), 
                 coefficients = coefs, sigma = sig, vcov = vcov(object), 
                 varcor = varcor, AICtab = llAIC[["AICtab"]], call = object$call), 
            class = "summary.glmmTMB")
}


# Function in which the error is thrown
VarCorr.glmmTMB <- function (x, sigma = 1, ...) 
{
  stopifnot(is.numeric(sigma), length(sigma) == 1)
  xrep <- x$obj$env$report(x$fit$parfull)
  reT <- x$modelInfo$reTrms
  reS <- x$modelInfo$reStruc
  familyStr <- family(x)$family
  useSc <- if (missing(sigma)) {
    sigma <- sigma(x)
    familyStr == "gaussian"
  }
  else TRUE
  vc.cond <- vc.zi <- NULL
  if (length(cn <- reT$cond$cnms)) {
    vc.cond <- glmmTMB:::mkVC(cor = xrep$corr, sd = xrep$sd, cnms = cn, 
                    sc = sigma, useSc = useSc)
    for (i in seq_along(vc.cond)) {
      attr(vc.cond[[i]], "blockCode") <- reS$condReStruc[[i]]$blockCode
    }
  }
  if (length(cn <- reT$zi$cnms)) {
    vc.zi <- glmmTMB:::mkVC(cor = xrep$corrzi, sd = xrep$sdzi, cnms = cn, 
                  sc = sigma, useSc = useSc)
    for (i in seq_along(vc.zi)) {   # Problem was in this line
      attr(vc.zi, "blockCode") <- reS$ziReStruc[[i]]$blockCode
    }
  }
  structure(list(cond = vc.cond, zi = vc.zi), sc = glmmTMB:::usesDispersion(familyStr), 
            class = "VarCorr.glmmTMB")
}
