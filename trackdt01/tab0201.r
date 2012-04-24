### tab0201: anova on d' as a function of probe delay and condition

f.tab0201 <- function () {
    thisfile <- "tab0201.r"
    infile <- "data02.rda"
    outfile <- "tab0201.txt"

    exit.function <- function () {
        while (sink.number() > 0) sink()
    }
    on.exit(exit.function())

    load(infile)

    sink(outfile)
    cat("ANOVA on d' as a function of probe delay and condition\n")
    print(summary(aov(dprime ~ (soa * cond) + Error(sub / (soa * cond)),
                      data02)))

    soa <- as.character(sort(as.numeric(levels(data02$soa))))
    cond <- as.character(sort(levels(data02$cond)))

    for (i in 1:(length(cond))) {
        cat("\n\n\n")
        cat("ANOVA on d' as a function of probe delay\n")
        cat("for condition", cond[i])
        print(summary(aov(dprime ~ soa + Error(sub / soa),
                          data02[data02$cond == cond[i], ])))
    }
}

f.tab0201()
rm(f.tab0201)
