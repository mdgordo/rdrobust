#' Quantile regression in regression discontinuity framework 
#' see Frandsen, Frolich and Melly (2010). "Quantile Treatment Effects in the Regression Discontinuity Design"
#'  
#' @param Y a numeric vector
#' @param x a numeric vector
#' @param fuzzy a binary vector indicating dummy for treatment
#' @param c a double for the cutoff point
#' @return a data frame of y values, coefficients, and standard errors
#' @export

rdquant <- function(Y, x, fuzzy = NULL, c = 0, ...){
        yvals <- quantile(Y, seq(.05, .95, .05))
        out_1 <- data.frame("yvals" = yvals,
                            "coef" = rep(NA, length(yvals)),
                            "se" = rep(NA, length(yvals)),
                            "treatment" = rep(1, length(yvals)))
        out_0 <- out_1
        out_0$treatment <- 0
        if (is.null(fuzzy)) fuzzy <- ifelse(Y>c, 1, 0)
        fuzzy0 <- 1 - fuzzy
        for (i in c(1:length(yvals))) {
                yi <- as.numeric(Y<yvals[i])
                y1d <- yi*fuzzy
                y0d <- yi*fuzzy0
                rd1out <- rdrobust(y = y1d, x = x, fuzzy = fuzzy, c = c, ...)
                rd0out <- rdrobust(y = y0d, x = x, fuzzy = fuzzy0, c = c, ...)
                out_1$coef[i] <- rd1out$coef["Conventional",]
                out_1$se[i] <- rd1out$se["Robust",]
                out_0$coef[i] <- rd0out$coef["Conventional",]
                out_0$se[i] <- rd0out$se["Robust",]
        }
        return(rbind(out_1, out_0))
}