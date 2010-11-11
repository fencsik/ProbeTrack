### tab1001: anova on RT as a function of probe delay and gap duration

f.tab1001 <- function () {
    infile <- "data10.rda"
    outfile <- "tab1001.txt"
    exit.function <- function () {
        while (sink.number() > 0) sink()
    }
    on.exit(exit.function())
    load(infile)

    sink(outfile)
    cat("ANOVA on RT as a function of probe delay and gap duration\n")
    print(summary(aov(rt ~ soa * gap + Error(sub / (soa * gap)),
                      data10)))
    cat("\n\n\nANOVA on RT as a function of probe delay\n")
    cat("  gap trials only\n")
    print(summary(aov(rt ~ soa + Error(sub / soa),
                      data10[data10$gap == "Gap", ])))
    cat("\n\n\nANOVA on RT as a function of probe delay\n")
    cat("  no-gap trials only\n")
    print(summary(aov(rt ~ soa + Error(sub / soa),
                      data10[data10$gap == "NoGap", ])))

    soa <- as.character(sort(as.numeric(levels(data10$soa))))

    cat("\n\n\nCompare pairs of SOAs on gap trials\n")
    for (i in 1:(length(soa) - 1)) {
        for (j in (i+1):length(soa)) {
            cat("\n\nCompare SOAs ", sprintf("%s and %s\n", soa[i], soa[j]))
            print(summary(aov(rt ~ soa + Error(sub / soa),
                              data10[data10$gap == "Gap" &
                                     (data10$soa == soa[i] |
                                      data10$soa == soa[j]), ])))
        }
    }

    cat("\n\n\nCompare pairs of SOAs on no-gap trials\n")
    for (i in 1:(length(soa) - 1)) {
        for (j in (i+1):length(soa)) {
            cat("\n\nCompare SOAs ", sprintf("%s and %s\n", soa[i], soa[j]))
            cat(sprintf("%s and %s\n", soa[i], soa[j]))
            print(summary(aov(rt ~ soa + Error(sub / soa),
                              data10[data10$gap == "NoGap" &
                                     (data10$soa == soa[i] |
                                      data10$soa == soa[j]), ])))
        }
    }

    cat("\n\n\nCompare gap and no-gap trials at each SOA\n")
    for (i in 1:length(soa)) {
        cat("\n\nAt SOA ", soa[i])
        print(summary(aov(rt ~ gap + Error(sub / gap),
                          data10[data10$soa == soa[i], ])))
    }
}

f.tab1001()
rm(f.tab1001)
