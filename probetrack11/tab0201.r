### tab0201: anova on d' as a function of probe delay and gap duration

f.tab0201 <- function () {
    infile <- "data02.rda"
    outfile <- "tab0201.txt"
    exit.function <- function () {
        while (sink.number() > 0) sink()
    }
    on.exit(exit.function())
    load(infile)

    sink(outfile)
    cat("ANOVA on d' as a function of probe delay and gap duration\n")
    print(summary(aov(dprime ~ soa * gap + Error(sub / (soa * gap)),
                      data02)))
    cat("\n\n\nANOVA on d' as a function of probe delay\n")
    cat("  gap trials only\n")
    print(summary(aov(dprime ~ soa + Error(sub / soa),
                      data02, subset = (gap == "Gap"))))
    cat("\n\n\nANOVA on d' as a function of probe delay\n")
    cat("  no-gap trials only\n")
    print(summary(aov(dprime ~ soa + Error(sub / soa),
                      data02, subset = (gap == "NoGap"))))

    soa <- as.character(sort(as.numeric(levels(data02$soa))))

    cat("\n\n\nCompare pairs of SOAs on gap trials\n")
    for (i in 1:(length(soa) - 1)) {
        for (j in (i+1):length(soa)) {
            cat("\n\nCompare SOAs ", sprintf("%s and %s\n", soa[i], soa[j]))
            print(summary(aov(dprime ~ soa + Error(sub / soa),
                              data02[data02$gap == "Gap" &
                                     (data02$soa == soa[i] |
                                      data02$soa == soa[j]), ])))
        }
    }

    cat("\n\n\nCompare pairs of SOAs on no-gap trials\n")
    for (i in 1:(length(soa) - 1)) {
        for (j in (i+1):length(soa)) {
            cat("\n\nCompare SOAs ", sprintf("%s and %s\n", soa[i], soa[j]))
            cat(sprintf("%s and %s\n", soa[i], soa[j]))
            print(summary(aov(dprime ~ soa + Error(sub / soa),
                              data02[data02$gap == "NoGap" &
                                     (data02$soa == soa[i] |
                                      data02$soa == soa[j]), ])))
        }
    }

    cat("\n\n\nCompare gap and no-gap trials at each SOA\n")
    for (i in 1:length(soa)) {
        cat("\n\nAt SOA ", soa[i])
        print(summary(aov(dprime ~ gap + Error(sub / gap),
                          data02[data02$soa == soa[i], ])))
    }
}

f.tab0201()
rm(f.tab0201)
