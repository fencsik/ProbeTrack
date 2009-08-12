### Analyze demographics

f.tab0001 <- function () {
    thisfile <- "tab0001.r"
    infile <- "data00.rda"
    demfile <- "demographics"
    outfile <- "tab0001.txt"

    on.exit( while (sink.number() > 0) sink() )

    if (!file.exists(infile)) stop("cannot open file ", infile)
    if (!file.exists(demfile)) stop("cannot open file ", demfile)
    if (IsFileUpToDate(outfile, c(thisfile, infile, demfile))) {
        warning("Output file is up to date, no action taken")
        return(invisible(NULL))
    }
    load(infile)
    if (!is.factor(data00$sub))
        data00$sub <- factor(data00$sub)
    subjects <- as.character(sort(as.numeric(levels(data00$sub))))
    
    demographics <- read.csv(demfile)
    sink(outfile)

    cat("All subjects in demographics file:\n")
    print(unique(demographics$sub))

    for (s in demographics$sub) {
        if (!any(s == subjects)) {
            demographics <- demographics[demographics$sub != s, ]
        }
    }

    cat("\nSubjects included in summary:\n")
    print(unique(demographics$sub))

    cat("\n\nDemographics summary\n")
    cat("\nAge:\n")
    print(summary(demographics$age))
    cat("\nSex:\n")
    print(summary(demographics$sex))

    sink()
}

f.tab0001()
rm(f.tab0001)
