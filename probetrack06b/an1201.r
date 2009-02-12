### an1201: anova to test differences among slopes as a function of probe delay
###
### $LastChangedDate$

f.an1201 <- function () {
    thisfile <- "an1201.r";
    infile <- "data12.rda";
    outfile <- "an1201.txt";

    exit.function <- function () {
        while (sink.number() > 0) sink();
    }
    on.exit(exit.function());

    if (!file.exists(infile)) stop("cannot open input file ", infile);
    if (IsFileUpToDate(outfile, c(thisfile, infile))) {
        warning("Output file is up to date, no action taken");
        return(invisible(NULL));
    }
    load(infile);
    r <- data12$regr;

    sink(outfile);
    cat("ANOVA testing effect of probe delay on slope\n");
    print(summary(aov(slope ~ soa + Error(sub / soa), data = r)));

    soaList <- levels(r$soa);
    n <- length(soaList);
    for (i in 1:(n-1)) {
        for (j in (i+1):n) {
            cat("\n\n\n");
            cat(sprintf("ANOVA comparing probe delays of %s and %s\n",
                        soaList[i], soaList[j]));
            print(summary(aov(slope ~ soa + Error(sub / soa), data = r,
                              subset = (r$soa == soaList[i] |
                                        r$soa == soaList[j]))));
        }
    }

    cat("\n\n\n");
    cat("ANOVA testing effect of probe delay on intercept\n");
    print(summary(aov(intercept ~ soa + Error(sub / soa), data = r)));

    for (i in 1:(n-1)) {
        for (j in (i+1):n) {
            cat("\n\n\n");
            cat(sprintf("ANOVA comparing probe delays of %s and %s\n",
                        soaList[i], soaList[j]));
            print(summary(aov(intercept ~ soa + Error(sub / soa),
                              data = r,
                              subset = (r$soa == soaList[i] |
                                        r$soa == soaList[j]))));
        }
    }
}

f.an1201();
rm(f.an1201);
