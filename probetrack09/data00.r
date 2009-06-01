### data00: cleans up raw data file
###
### $LastChangedDate$

f.data00 <- function () {
    infile <- "rawdata.txt"
    outfile <- "data00.rda"
    thisfile <- "data00.r"

    if (!file.exists(infile)) stop("cannot open file ", infile)
    if (IsFileUpToDate(outfile, c(thisfile, infile))) {
        warning("Output file is up to date, no action taken")
        return(invisible(NULL))
    }
    data00 <- read.delim(infile)

### remove bad subjects:
    ## 1. Subject 2, 3, 6, and 15 had accuracy less that 80% in at least one
    ## SOA condition
    data00 <- data00[data00$sub != 2 & data00$sub != 3 & data00$sub != 6 &
                     data00$sub != 15, ]

### remove practice blocks, bad keypresses, and RTs <= 0
    data00 <- data00[data00$prac == 0 & data00$acc >= 0 & data00$rt > 0, ]
    data00$blocktype <- factor(data00$blocktype)
    data00$resp <- factor(data00$resp)

### rename and recode variables
    ## probeType == 1 -> target; probeType == 2 -> distractor
    data00$target <- factor(data00$probeTarget, levels = c(0, 1),
                            labels = c("distractor", "target"))

    ## clean-up soa and convert to factor
    data00$soa <- data00$soa * 1000 / 75 # convert to ms
    data00$soa <- factor(data00$soa, levels = sort(unique(data00$soa)))
    ## convert ntargets to factor
    data00$ntargets <- factor(data00$ntargets,
                              levels = sort(unique(data00$ntargets)))

    save(data00, file=outfile)
}

f.data00()
rm(f.data00)
