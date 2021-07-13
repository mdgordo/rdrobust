#' Quantile regression in regression discontinuity framework 
#' see Frandsen, Frolich and Melly (2010). "Quantile Treatment Effects in the Regression Discontinuity Design"
#'  
#' @param Y a numeric vector
#' @param grid an integer determining how finely to estimate pdfs
#' @return a data frame of y values, coefficients, and standard errors
#' @export

rdquant <- function(Y, grid = 50, ...){
        yvals <- seq(min(Y), max(Y), length.out = grid)
        yvals <- yvals[c(2:(grid - 1))]
        out <- data.frame("yvals" = yvals,
                          "coef" = c(),
                          "se" = c())
        for (i in c(1:grid)) {
                yi <- as.numeric(Y>yvals[i])
                rdout <- rdrobust(y = yi, ...)
                out$coef[i] <- rdout$coef["Conventional",]
                out$se[i] <- rdout$se["Robust",]
        }
        return(out)
}