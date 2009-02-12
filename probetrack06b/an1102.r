### an1102: anova to test differences among parameters as a function of tracking
### load
###
### $LastChangedDate$

f.an1102 <- function () {
    thisfile <- "an1102.r";
    infile <- "data11.rda";
    outfile <- "an1102.txt";

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
    p <- data11$parameters;

    sink(outfile);
    cat("ANOVA testing effect of tracking load on rtime\n");
    print(summary(aov(rtime ~ ntargets + Error(sub / ntargets), data = p)));

    ntargetsList <- levels(p$ntargets);
    n <- length(ntargetsList);
    for (i in 1:(n-1)) {
        for (j in (i+1):n) {
            cat("\n\n\n");
            cat(sprintf("ANOVA comparing tracking loads of %s and %s\n",
                        ntargetsList[i], ntargetsList[j]));
            print(summary(aov(rtime ~ ntargets + Error(sub / ntargets), data = p,
                              subset = (p$ntargets == ntargetsList[i] |
                                        p$ntargets == ntargetsList[j]))));
        }
    }

    cat("\n\n\n");
    cat("ANOVA testing effect of tracking load on baseRT\n");
    print(summary(aov(baseRT ~ ntargets + Error(sub / ntargets), data = p)));

    for (i in 1:(n-1)) {
        for (j in (i+1):n) {
            cat("\n\n\n");
            cat(sprintf("ANOVA comparing tracking loads of %s and %s\n",
                        ntargetsList[i], ntargetsList[j]));
            print(summary(aov(baseRT ~ ntargets + Error(sub / ntargets),
                              data = p,
                              subset = (p$ntargets == ntargetsList[i] |
                                        p$ntargets == ntargetsList[j]))));
        }
    }
}

f.an1102();
rm(f.an1102);
