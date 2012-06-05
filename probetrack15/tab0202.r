### tab0202: anova comparing d' between pairs of probe delays within each
### gap type

f.tab0202 <- function () {
    infile <- "data02.rda"
    outfile <- "tab0202.txt"

    exit.function <- function () {
        while (sink.number() > 0) sink()
    }
    on.exit(exit.function())

    load(infile)

    sink(outfile)
    soa <- as.character(sort(as.numeric(levels(data02$soa))))
    cond <- as.character(sort(levels(data02$cond)))
    for (i in 1:(length(cond))) {
        cat("\n\n\n")
        cat(sprintf("ANOVA on d' as a function of probe delay for condition %s\n",
                    cond[i]))
        for (j in 1:(length(soa)-1)) {
            for (k in (j+1):length(soa)) {
                soa1 <- soa[j]
                soa2 <- soa[k]
                cat(sprintf("\n\nSOA: %s vs. %s\n\n", soa1, soa2))
                print(summary(aov(dprime ~ soa + Error(sub / soa),
                                  data02[data02$cond == cond[i] &
                                         (data02$soa == soa1 | data02$soa == soa2), ])))
            }
        }
    }
}

f.tab0202()
rm(f.tab0202)
