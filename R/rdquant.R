### Quantile regression in regression discontinuity framework 
## see Frandsen, Frolich and Melly (2010). "Quantile Treatment Effects in the Regression Discontinuity Design"
## grid controls how finely to estimate pdf
## returns a data.frame of y values, coefficient estimates, and standard errors 

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