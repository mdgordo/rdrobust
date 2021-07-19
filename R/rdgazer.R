#' Function for putting rdrobust output into stargazer format
#' 
#' 
#' @param rdlist takes a list of rdrobust objects
#' @param dvlabs takes a character vector of dependent variable labels
#' @param xlines passes extra lines to stargazer's add lines argument
#' @return a stargazer object
#' @export

rdgazer <- function(rdlist, dvlabs = NULL, xlines = NULL, ...){
        dummymods <- list(); coef <- list(); se <- list(); bw <- c(); nobs <- c(); untreatedmean <- c()
        for (i in 1:length(rdlist)) {
                dummymods[[i]] <- lm(rdlist[[i]]$Y ~ rdlist[[i]]$X)
                coef[[i]] <- c(0, rdlist[[i]]$coef["Conventional",])
                se[[i]] <- c(1, rdlist[[i]]$se["Robust",])
                bw[i] <- round(rdlist[[i]]$bws[1,1],2)
                nobs[i] <- sum(rdlist[[i]]$N_h)
                untreatedmean[i] <- round(rdlist[[i]]$beta_p_l,2)
        }
        s <- stargazer(dummymods, type = "text", coef = coef, se = se, column.labels = dvlabs,
                       omit.stat = "all", digits = 2, df = FALSE, omit = c("Constant"), 
                       covariate.labels = c("Treatment"),
                       add.lines = c(list(c("N", nobs),
                                          c("Bandwidth", bw),
                                          c("Untreated Estimate", untreatedmean)), xlines), ...)
}