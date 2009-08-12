### ANOVA comparing ProbeTrack02 to ProbeTrack09

f.tab1002 <- function () {
    thisfile <- "tab1002.r"
    infile2 <- "../probetrack02/data10.rda"
    infile9 <- "data10.rda"
    outfile <- "tab1002.txt"

    exit.function <- function () {
        while (sink.number() > 0) sink()
    }
    on.exit(exit.function())

    if (!file.exists(infile2)) stop("cannot open input file ", infile2)
    if (!file.exists(infile9)) stop("cannot open input file ", infile9)
    if (IsFileUpToDate(outfile, c(thisfile, infile2, infile9))) {
        warning("Output file is up to date, no action taken")
        return(invisible(NULL))
    }
    load(infile2)
    dt2 <- data10
    load(infile9)
    dt9 <- data10

    ## clean up data files
    dt2 <- dt2[dt2$gapdur > 0, ]
    if (is.factor(dt2$soa)) dt2$soa <- as.numeric(as.character(dt2$soa))
    if (is.factor(dt9$soa)) dt9$soa <- as.numeric(as.character(dt9$soa))
    if (is.factor(dt2$sub)) dt2$sub <- as.character(dt2$sub)
    if (is.factor(dt9$sub)) dt9$sub <- as.character(dt9$sub)

    ## ensure the same column headings
    columns <- unique(c(colnames(dt2), colnames(dt9)))
    for (col in columns) {
        if (!any(colnames(dt2) == col) || !any(colnames(dt9) == col)) {
            dt2 <- dt2[, colnames(dt2) != col]
            dt9 <- dt9[, colnames(dt9) != col]
        }
    }
    dt9 <- dt9[, colnames(dt2)]

    ## filter SOAs
    soalist <- sort(unique(c(unique(dt2$soa), unique(dt9$soa))))
    for (soa in soalist) {
        if (!any(dt2$soa == soa) || !any(dt9$soa == soa)) {
            dt2 <- dt2[dt2$soa != soa, ]
            dt9 <- dt9[dt2$soa != soa, ]
        }
    }

    dt2$exp <- "ProbeTrack02"
    dt9$exp <- "ProbeTrack09"

    dt <- rbind(dt2, dt9)
    dt$soa <- factor(dt$soa)
    dt$sub <- factor(dt$sub)
    dt$exp <- factor(dt$exp)

    sink(outfile)
    cat("ANOVA on median correct RT as a function of probe delay and experiment\n")
    print(summary(aov(rt ~ soa * exp + Error(sub / soa), dt)))
}

f.tab1002()
rm(f.tab1002)
