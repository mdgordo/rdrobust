#' Quantile regression in regression discontinuity framework 
#' see Frandsen, Frolich and Melly (2010). "Quantile Treatment Effects in the Regression Discontinuity Design"
#'  
#' @param Y a numeric vector
#' @param x a numeric vector
#' @param fuzzy a binary vector indicating dummy for treatment
#' @param c a double for the cutoff point
#' @param grid a double between 0 and 1 controlling how finely to estimate the pdfs
#' @param qstep a double between 0 and 1 controlling which quantile treatment effects to return
#' @return list of coefficients, standard errors and quantile treatment effect standard errors
#' @export

rdquant <- function(Y, x, fuzzy = NULL, c = 0, grid = .01, qstep = .05, indices, ...){
        if (is.null(fuzzy)) fuzzy <- ifelse(Y>c, 1, 0)
        fuzzy0 <- 1 - fuzzy
        yvals1 <- quantile(Y[fuzzy==1], seq(.05, .95, grid))
        yvals0 <- quantile(Y[fuzzy0==1], seq(.05, .95, grid))
        coefs <- function(qinv, f){
                y1d <- as.numeric(Y<qinv)*f
                rd1out <- rdrobust(y = y1d, x = x, fuzzy = f, c = c, ...)
                return(rd1out)
        }
        mods1 <- lapply(yvals1, FUN = coefs, f = fuzzy)
        mods0 <- lapply(yvals0, FUN = coefs, f = fuzzy0)
        coefs1 <- lapply(mods1, function(x) x$coef["Conventional",])
        coefs0 <- lapply(mods0, function(x) x$coef["Conventional",])
        ses1 <- lapply(mods1, function(x) x$se["Robust",])
        ses0 <- lapply(mods0, function(x) x$se["Robust",])
        r1 <- rearrangement(x = data.frame(yvals1), y = coef1)
        r0 <- rearrangement(x = data.frame(yvals0), y = coef0)
        q1 <- sapply(seq(.05, .95, qstep), function(x) min(yvals1[r1>x]))
        q0 <- sapply(seq(.05, .95, qstep), function(x) min(yvals0[r0>x]))
        q <- q1 - q0
        return(list(coefs1, coefs0, ses1, ses0, q))
}
