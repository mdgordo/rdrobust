## Function for putting rdrobust output into stargazer format
# rdlist takes a list of rdrobust objects
# 

rdgazer <- function(rdlist, dvlabs = c(1:length(rdlist)), xlines = NULL, ...){
        dummymods <- list(); coef <- list(); se <- list(); bw <- c(); nobs <- c(); dvlabs <- c(); depvarmean <- c()
        for (i in 1:length(rdlist)) {
                dummymods[[i]] <- lm(rdlist[[i]]$Y ~ rdlist[[i]]$X)
                coef[[i]] <- c(0, rdlist[[i]]$coef["Conventional",])
                se[[i]] <- c(1, rdlist[[i]]$se["Robust",], 1)
                bw[i] <- round(rdlist[[i]]$bws[1,1],2)
                nobs[i] <- sum(rdlist[[i]]$N_h)
                depvarmean[i] <- round(weighted.mean(rdlist[[i]]$Y, rdlist[[i]]$W, na.rm = TRUE),2)
        }
        s <- stargazer(dummymods, type = "text", coef = coef, se = se, column.labels = dvlabs,
                       omit.stat = "all", digits = 2, df = FALSE, omit = c("Constant"), 
                       covariate.labels = c("Treatment"),
                       add.lines = c(list(c("N", nobs),
                                          c("Bandwidth", bw),
                                          c("Dep Var Mean", depvarmean)), xlines), ...)
}