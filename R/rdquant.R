#' Quantile regression in regression discontinuity framework 
#' see Frandsen, Frolich and Melly (2010). "Quantile Treatment Effects in the Regression Discontinuity Design"
#'  
#' @param Y a numeric vector
#' @param x a numeric vector
#' @param fuzzy a binary vector indicating dummy for treatment
#' @param c a double for the cutoff point
#' @param grid a double between 0 and 1 controlling how finely to estimate the pdfs
#' @param qstep a double between 0 and 1 controlling which quantile treatment effects to return
#' @return a dataframe of coefficients, standard errors and quantile treatment effect standard errors, and rearranged estimates, and a vector of quantile treatment effect estimates
#' @export

rdquant <- function(Y, x, fuzzy = NULL, c = 0, grid = .01, qstep = .05, ...){
        if (is.null(fuzzy)) fuzzy <- ifelse(x>c, 1, 0)
        fuzzy0 <- 1 - fuzzy
        yvals1 <- quantile(Y[fuzzy==1], seq(.05, .95, grid), na.rm = TRUE)
        yvals0 <- quantile(Y[fuzzy0==1], seq(.05, .95, grid), na.rm = TRUE)
        coefs <- function(qinv, f){
                y1d <- as.numeric(Y<=qinv)*f
                rd1out <- rdrobust(y = y1d, x = x, fuzzy = f, c = c, ...)
                return(rd1out)
        }
        mods1 <- lapply(yvals1, FUN = coefs, f = fuzzy)
        mods0 <- lapply(yvals0, FUN = coefs, f = fuzzy0)
        coefs1 <- as.numeric(lapply(mods1, function(x) x$coef["Conventional",]))
        coefs0 <- as.numeric(lapply(mods0, function(x) x$coef["Conventional",]))
        ses1 <- as.numeric(lapply(mods1, function(x) x$se["Robust",]))
        ses0 <- as.numeric(lapply(mods0, function(x) x$se["Robust",]))
        r1 <- Rearrangement::rearrangement(x = data.frame(yvals1), y = coefs1)
        r0 <- Rearrangement::rearrangement(x = data.frame(yvals0), y = coefs0)
        ci1 <- list("x" = yvals1, "y" = coefs1, "sortedx" = yvals1, "Lower" = coefs1 - ses1*1.96, "Upper" = coefs1 + ses1*1.96, "cef" = coefs1)
        attr(ci1, "class") <- "conint"
        rci1 <- rconint(ci1)
        ci0 <- list("x" = yvals0, "y" = coefs0, "sortedx" = yvals0, "Lower" = coefs0 - ses0*1.96, "Upper" = coefs0 + ses0*1.96, "cef" = coefs0)
        attr(ci0, "class") <- "conint"
        rci0 <- rconint(ci0)
        q1 <- sapply(seq(.05, .95, qstep), function(x) min(yvals1[r1>x]))
        q0 <- sapply(seq(.05, .95, qstep), function(x) min(yvals0[r0>x]))
        q <- q1 - q0 
        pdfdf <- data.frame("yvals" = c(yvals1, yvals0),
                            "coefs" = c(coefs1, coefs0),
                            "se" = c(ses1, ses0),
                            "rcoefs" = c(r1, r0),
                            "rlower" = c(rci1$Lower, rci0$Lower),
                            "rupper" = c(rci1$Upper, rci0$Upper),
                            "treat" = c(rep(1, length(yvals1)), rep(0, length(yvals0))))
        return(list("pdfs" = pdfdf, "qte" = q))
}
