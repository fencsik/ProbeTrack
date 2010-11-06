### an1001: anova on median correct RT as a function of condition and probe
### delay

f.an1001 <- function () {
    thisfile <- "tab1001.r"
    infile <- "data10.rda"
    outfile <- "tab1001.txt"

    exit.function <- function () {
        while (sink.number() > 0) sink()
    }
    on.exit(exit.function())

    load(infile)

    sink(outfile)
    cat("ANOVA on median correct RT as a function of probe delay\n")
    cat("and condition")
    print(summary(aov(rt ~ cond * soa + Error(sub / (cond * soa)),
                      data10)))

    soa <- as.character(sort(as.numeric(levels(data10$soa))))
    cond <- as.character(sort(levels(data10$cond)))

    for (i in 1:(length(cond))) {
        cat("\n\n\n")
        cat("ANOVA on median correct RT as a function of probe delay\n")
        cat("for condition", cond[i])
        print(summary(aov(rt ~ soa + Error(sub / soa),
                          data10[data10$cond == cond[i], ])))
    }
}

f.an1001()
rm(f.an1001)
