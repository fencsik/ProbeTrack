### tab1101: generate table of parameters of fitted model for each subject
###
### $LastChangedDate$

f.tab1101 <- function () {
    thisfile <- "tab1101.r"
    infile <- "data11.rda"
    outfile <- "tab1101.txt"

    on.exit( while (sink.number() > 0) sink() )

    if (!file.exists(infile)) stop("cannot open input file ", infile)
    if (IsFileUpToDate(outfile, c(thisfile, infile))) {
        warning("Output file is up to date, no action taken")
        return(invisible(NULL))
    }
    load(infile)

    ## Compute mean and 95% CI on fit parameters
    dt <- data11$fit
    dt$sub <- as.character(dt$sub)
    dt$r.sq <- dt$r^2
    rownames(dt) <- subjects <- dt$sub
    dt <- dt[, colnames(dt) != "sub"] # remove non-parameter columns
    dt2 <- rbind(dt, apply(as.matrix(dt), 2, mean, na.rm = TRUE))
    dt2 <- rbind(dt2, apply(as.matrix(dt), 2,
                            function(x, na.rm) {
                                x <- x[!is.na(x)]
                                qt(.975, length(x) - 1) *
                                    sqrt(var(x) / length(x))
                            }
                            ))
    rownames(dt2) <- c(subjects, "MEAN", "CI")
    dt <- dt2

    sink(outfile)
    cat("Parameter estimates from model fit\n")
    print(round(y <- dt[, c("rtime", "baseRT")], 3))

    cat("\n\n\n")
    cat("Fitting output and goodness-of-fit statistics\n")
    print(round(dt[, c("iter", "code", "sse", "rmse", "r.sq")], 3))

    cat("\n\n\n")
    cat("Average rtime, with 95% confidence intervals\n")
    M <- dt["MEAN", "rtime"]
    CI <- dt["CI", "rtime"]
    cat(sprintf("   %0.2f +/- %0.2f (%0.2f, %0.2f)\n",
                M, CI, M - CI, M + CI))

    cat("\n\n\n")
    cat("Average baseRT, with 95% confidence intervals\n")
    M <- dt["MEAN", "baseRT"]
    CI <- dt["CI", "baseRT"]
    cat(sprintf("   %0.2f +/- %0.2f (%0.2f, %0.2f)\n",
                M, CI, M - CI, M + CI))

    cat("\n\n\n")
    cat("Goodness of fit statistics for overall data\n")
    cat(with(data11$data,
             sprintf("R-squared = %0.5f\n", cor(rt, rt.pred)^2)))

    cat("\n\n\n")
    cat("Goodness of fit statistics for averaged fit\n")
    obsey <- with(data11$data, aggregate(rt, list(soa), mean))$x
    predy <- with(data11$data, aggregate(rt.pred, list(soa), mean))$x
    cat(sprintf("R-squared = %0.5f\n", cor(obsey, predy) ^ 2))

    cat("\n\n\n")
    cat("Goodness of fit statistics for fit from averaged parameters\n")
    m.rtime <- dt["MEAN", "rtime"]
    m.baseRT <- dt["MEAN", "baseRT"]
    obse <- with(data11$data, tapply(rt, list(soa), mean))
    pred <- with(data11$data, tapply(rt.pred, list(soa), mean))
    x <- as.numeric(dimnames(obse)[[1]])
    pred <- data11$model(x, m.rtime, m.baseRT)
    cat(sprintf("R-squared = %0.5f\n", cor(as.vector(obse), as.vector(pred))^2))
}

f.tab1101()
rm(f.tab1101)
