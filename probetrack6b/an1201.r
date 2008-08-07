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
    cat("ANOVA testing effect of tracking load on slope\n");
    print(summary(aov(slope ~ ntargets + Error(sub / ntargets), data = r)));

    ntargetsList <- levels(r$ntargets);
    n <- length(ntargetsList);
    for (i in 1:(n-1)) {
        for (j in (i+1):n) {
            cat("\n\n\n");
            cat(sprintf("ANOVA comparing tracking loads of %s and %s\n",
                        ntargetsList[i], ntargetsList[j]));
            print(summary(aov(slope ~ ntargets + Error(sub / ntargets), data = r,
                              subset = (r$ntargets == ntargetsList[i] |
                                        r$ntargets == ntargetsList[j]))));
        }
    }

    cat("\n\n\n");
    cat("ANOVA testing effect of tracking load on intercept\n");
    print(summary(aov(intercept ~ ntargets + Error(sub / ntargets), data = r)));

    for (i in 1:(n-1)) {
        for (j in (i+1):n) {
            cat("\n\n\n");
            cat(sprintf("ANOVA comparing tracking loads of %s and %s\n",
                        ntargetsList[i], ntargetsList[j]));
            print(summary(aov(intercept ~ ntargets + Error(sub / ntargets),
                              data = r,
                              subset = (r$ntargets == ntargetsList[i] |
                                        r$ntargets == ntargetsList[j]))));
        }
    }
}

f.an1201();
rm(f.an1201);
