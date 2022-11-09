# convert original tau in bootstrap results into half-lifes
# replaces tau_1, tau_2, ... with HL_1, HL_2
convertHalfLife <- function(df) {
  cols <- grep("tau", names(df))
  df[, cols] <- log(2) * df[, cols]
  names(df) <- gsub("tau", "HL", names(df))
  df
}

# read bootstrap data created by rule boot_all in the snakefile
# you need to specify the total number of batches, nbatch
# halflife is a logical to indicate conversion of taus into HLs
readBootstrap <- function(set, bootDir, nbatch, root, halflife) {
  dat <- NULL
  for(batch in 1:nbatch) {
    file <- file.path(bootDir, str_glue("{root}_{set}_{batch}.tsv"))
    if(file.exists(file)) {
      info <- file.info(file)
      if(info$size > 0) {
        df <- read.table(file, header = TRUE, sep="\t")
        if(halflife) {
          df <- convertHalfLife(df)
        }
        dat <- rbind(dat, df)
      } else {
        warning(paste("file", file, "is empty."))
      }
    } else {
      warning(paste("File", file, "does not exist."))
    }
  }
  dat
}


# read boootstraps for all sets, return a list of data frames
# this is a simple wrapper around 'readBootstrap'
readBootstraps <- function(sets, bootDir, nboot=300, root="boot", halflife=TRUE) {
  boot <- lapply(sets, function(set) {
    readBootstrap(set, bootDir, nboot, root, halflife)
  })
  names(boot) <- sets
  boot
}


# find median, 95% CI and 25-27% quantiles
# dat is a data frame with bootstrap results for one set
bootParamStats <- function(dat, pars=c("HL1", "HL2", "k1", "k2")) {
  df <- NULL
  for(par in pars) {
    q <- quantile(dat[[par]], probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
    row <- data.frame(par=par, median=q[3], lo95=q[1], up95=q[5], p25=q[2], p75=q[4])
    df <- rbind(df, row)
  }
  df
}


# plot histograms of bootstrap data
# dat is a data frame with bootstrap results for one set
bootParamPlot <- function(dat, title="", pars=c("HL1", "HL2", "k1", "k2")) {
  # dummy data for x-limits
  vars <- c("HL1", "HL2", "k1", "k2")
  dummy <- data.frame(
    variable = c(vars, vars),
    value = c(3, 3, 0, 0, 20, 20, 0.2, 0.2)
  )
  dummy <- dummy[dummy$variable %in% pars,]
  
  m <- reshape2::melt(dat, measure.vars=pars)
  ggplot(m, aes(value)) +
    geom_blank(data=dummy) +
    geom_histogram(bins=50) +
    facet_wrap(~variable, scales="free", nrow=1) +
    labs(title=title)
}


# create a data frame with bootstrap best-fitting parameters (medians)
# boots - a list of data frames with bootstrap results for all sets
# sets - a list of sets to use
# pars - a vector of parameter names to use
bootstrapParamsTab <- function(boot, sets=NULL, pars=c("HL1", "HL2", "k1", "k2"), format="%.3g") {
  if(is.null(sets)) sets <- names(boot)
  P <- lapply(sets, function(set) {
    bp <- bootParamStats(boot[[set]], pars)
    sprintf(format, bp$median)
  })
  names(P) <- sets
  df <- plyr::ldply(P, .id="set")
  colnames(df) <- c("set", pars)
  df
}


# create a data frame with bootstrap best-fitting parameters, 95% CIs and quartiles
# boots - a list of data frames with bootstrap results for all sets
# sets - a list of sets to use
# pars - a vector of parameter names to use
bootstrapParamsTabCI <- function(boot, sets=NULL, pars=c("HL1", "HL2", "k1", "k2"), format="%.3g") {
  if(is.null(sets)) sets <- names(boot)
  P <- lapply(sets, function(set) {
    bp <- bootParamStats(boot[[set]], pars)
    c(
      sprintf(format, bp$median),
      sprintf(format, bp$lo95),
      sprintf(format, bp$up95),
      sprintf(format, bp$p25),
      sprintf(format, bp$p75)
    )
  })
  names(P) <- sets
  df <- plyr::ldply(P, .id="set")
  colnames(df) <- c("set", paste0(pars, "_med"), paste0(pars, "_lo95"), paste0(pars, "_up95"), paste0(pars, "_p25"), paste0(pars, "_p75"))
  df
}


# plot bootstrap distributions for all sets
# boots - a list of data frames with bootstrap results for all sets
# sets - a list of sets to use
# pars - a vector of parameter names to use
plotBootstrapDistributions <- function(boot, sets=NULL, pars=c("HL1", "HL2", "k1", "k2")) {
  if(is.null(sets)) sets <- names(boot)
  P <- lapply(sets, function(set) {
    bootParamPlot(boot[[set]], set, pars)
  })
  gridExtra::grid.arrange(grobs=P, ncol=1) 
}


# plot correlation of selected bootstrap parameters
# boots - a list of data frames with bootstrap results for all sets
# sets - a list of sets to use
# pars - a vector of two parameter names to use
plotBootstrapParamCor <- function(boot, sets=NULL, pars=c("HL2", "k2")) {
  if(is.null(sets)) sets <- names(boot)
  md <- plyr::ldply(boot, .id="set")
  md <- md[,c("set", pars)]
  
  ggplot(md, aes_string(x=pars[1], y=pars[2])) +
    geom_point(shape=1) +
    geom_smooth(method="loess", se=FALSE, colour="red") +
    facet_wrap(~set)
}


# plot boostrap best-fitting values with 95% CIs
# boots - a list of data frames with bootstrap results for all sets
# sets - a list of sets to use
# pars - a vector of parameter names to use
plotBootstrapCI <- function(boot, sets=NULL, pars=c("HL1", "HL2", "k1", "k2")) {
  if(is.null(sets)) sets <- names(boot)
  P <- lapply(sets, function(set) {
    bootParamStats(boot[[set]], pars)
  })
  names(P) <- sets
  df <- plyr::ldply(P, .id="set")
  df$set <- as.factor(df$set)
  
  ggplot(df, aes(set, median)) +
    geom_errorbar(aes(x=set, ymin=lo95, ymax=up95), width=0.4) +
    geom_point() +
    facet_wrap(~par, scales="free_y", nrow=1) +
    theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)) +
    labs(x="Set", y="Fit parameter value") 
}


# plot bootstrap box plots for all data
# boots - a list of data frames with bootstrap results for all sets
# sets - a list of sets to use
# pars - a vector of parameter names to use
plotBootstrapBox <- function(boot, pars=c("HL1", "HL2", "k1", "k2")) {
  md <- plyr::ldply(boot, .id="set")
  md <- md[,c("set", pars)]
  m <- reshape2::melt(md, id.var="set")
  
  ggplot(m, aes(set, value)) +
    geom_boxplot(outlier.size = 0.1, outlier.colour="grey", position=position_dodge(width=0.8)) +
    facet_wrap(~variable, scales="free_y", nrow=1) +
    theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)) +
    labs(x="Set", y="Fit parameter value")
}
