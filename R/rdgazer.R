#' Function for putting rdrobust output into stargazer format
#' 
#' 
#' @param rdlist takes a list of rdrobust objects
#' @param dvlabs takes a character vector of dependent variable labels
#' @param xlines passes extra lines to stargazer's add lines argument
#' @param se_r character either "Conventional" or "Robust"
#' @return a stargazer object
#' @export

rdgazer <- function(rdlist, dvlabs = NULL, xlines = NULL, se_r = "Conventional", type = "text", ...){
        dummymods <- list(); coef <- list(); se <- list(); bw <- c(); nobs <- c(); untreatedmean <- c()
        for (i in 1:length(rdlist)) {
                dummymods[[i]] <- lm(rdlist[[i]]$Y ~ rdlist[[i]]$X)
                coef[[i]] <- c(0, rdlist[[i]]$coef["Conventional",])
                se[[i]] <- c(1, rdlist[[i]]$se[se_r,])
                bw[i] <- round(rdlist[[i]]$bws[1,1],2)
                nobs[i] <- sum(rdlist[[i]]$N_h)
        }
        s <- stargazer(dummymods, type = type, coef = coef, se = se, column.labels = dvlabs,
                       omit.stat = "all", digits = 2, df = FALSE, omit = c("Constant"), 
                       covariate.labels = c("Treatment"),
                       add.lines = c(list(c("N", nobs),
                                          c("Bandwidth", bw)), xlines), ...)
}
