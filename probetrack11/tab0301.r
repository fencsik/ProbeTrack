### tab0301: anova on RT as a function of probe delay and gap duration

f.tab0301 <- function () {
    infile <- "data03.rda"
    outfile <- "tab0301.txt"
    exit.function <- function () {
        while (sink.number() > 0) sink()
    }
    on.exit(exit.function())
    load(infile)

    sink(outfile)
    cat("ANOVA on RT as a function of probe delay and gap duration\n")
    print(summary(aov(rt ~ soa * gap + Error(sub / (soa * gap)),
                      data03)))
    cat("\n\n\nANOVA on RT as a function of probe delay\n")
    cat("  gap trials only\n")
    print(summary(aov(rt ~ soa + Error(sub / soa),
                      data03[data03$gap == "Gap", ])))
    cat("\n\n\nANOVA on RT as a function of probe delay\n")
    cat("  no-gap trials only\n")
    print(summary(aov(rt ~ soa + Error(sub / soa),
                      data03[data03$gap == "NoGap", ])))

    soa <- as.character(sort(as.numeric(levels(data03$soa))))

    cat("\n\n\nCompare pairs of SOAs on gap trials\n")
    for (i in 1:(length(soa) - 1)) {
        for (j in (i+1):length(soa)) {
            cat("\n\nCompare SOAs ", sprintf("%s and %s\n", soa[i], soa[j]))
            print(summary(aov(rt ~ soa + Error(sub / soa),
                              data03[data03$gap == "Gap" &
                                     (data03$soa == soa[i] |
                                      data03$soa == soa[j]), ])))
        }
    }

    cat("\n\n\nCompare pairs of SOAs on no-gap trials\n")
    for (i in 1:(length(soa) - 1)) {
        for (j in (i+1):length(soa)) {
            cat("\n\nCompare SOAs ", sprintf("%s and %s\n", soa[i], soa[j]))
            cat(sprintf("%s and %s\n", soa[i], soa[j]))
            print(summary(aov(rt ~ soa + Error(sub / soa),
                              data03[data03$gap == "NoGap" &
                                     (data03$soa == soa[i] |
                                      data03$soa == soa[j]), ])))
        }
    }

    cat("\n\n\nCompare gap and no-gap trials at each SOA\n")
    for (i in 1:length(soa)) {
        cat("\n\nAt SOA ", soa[i])
        print(summary(aov(rt ~ gap + Error(sub / gap),
                          data03[data03$soa == soa[i], ])))
    }
}

f.tab0301()
rm(f.tab0301)
