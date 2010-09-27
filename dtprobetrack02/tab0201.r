### tab0201: anova on d' as a function of probe delay and number of targets
###
### $LastChangedDate$

f.tab0201 <- function () {
    thisfile <- "tab0201.r"
    infile <- "data02.rda"
    outfile <- "tab0201.txt"

    exit.function <- function () {
        while (sink.number() > 0) sink()
    }
    on.exit(exit.function())

    if (!file.exists(infile)) stop("cannot open input file ", infile)
    if (IsFileUpToDate(outfile, c(thisfile, infile))) {
        warning("Output file is up to date, no action taken")
        return(invisible(NULL))
    }
    load(infile)

    data02$gapdur <- factor(data02$gapdur)

    sink(outfile)
    cat("ANOVA on d' as a function of probe delay\n")
    cat("  gap trials only\n")
    print(summary(aov(dprime ~ soa + Error(sub / soa),
                      data02)))

    soa <- as.character(sort(as.numeric(levels(data02$soa))))
    for (i in 1:(length(soa) - 1)) {
        for (j in (i+1):length(soa)) {
            cat("\n\n\n")
            cat("ANOVA on d' comparing SOAs ")
            cat(sprintf("%s and %s\n", soa[i], soa[j]))
            print(summary(aov(dprime ~ soa + Error(sub / soa),
                              data02[data02$soa == soa[i] |
                                     data02$soa == soa[j], ])))
        }
    }
}

f.tab0201()
rm(f.tab0201)
