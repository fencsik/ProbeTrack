### an1202: anova on median correct RT as a function of probe delay and gap
### duration
###
### $LastChangedDate$

do.an1202 <- function () {
    thisfile <- "an1202.r";
    infile <- "data12.rda";
    outfile <- "an1202.txt";

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

    data12 <- data12$data
    data12$soa <- factor(data12$soa)
    data12$sub <- factor(data12$sub)

    sink(outfile);
    cat("ANOVA on median correct RT as a function of probe delay\n");
    cat("  gap trials only\n");
    print(summary(aov(rt ~ soa + Error(sub / soa), data12)));
}

do.an1202();
rm(do.an1202);
