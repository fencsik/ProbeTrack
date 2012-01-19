### tab1001: anova on median correct RT as a function of probe delay

f.tab1001 <- function () {
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
    print(summary(aov(rt ~ soa + Error(sub / (soa)),
                      data10)))

    soa <- as.character(sort(as.numeric(levels(data10$soa))))

    for (i in 1:(length(soa)-1)) {
        for (j in (i+1):(length(soa))) {
            cat("\n\n\n")
            cat("ANOVA on median correct RT as a function of probe delay\n")
            cat("comparing SOA", soa[i], "and", soa[j], "\n")
            print(summary(aov(rt ~ soa + Error(sub / soa),
                          data10[data10$soa == soa[i] | data10$soa == soa[j], ])))
        }
    }
}

f.tab1001()
rm(f.tab1001)
