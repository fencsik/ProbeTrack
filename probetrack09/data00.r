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

    sub.pre <- length(unique(data00$sub))
### remove bad subjects:
    ## 1. Subject 2, 3, 6, and 15 had accuracy less that 80% in at least
    ## one SOA condition
    ## 2. Subject 8 has unusually high false-alarm rates
    ## data00 <- data00[data00$sub != 2 & data00$sub != 3 &
    ##                 data00$sub != 6 & data00$sub != 15, ]
    ## data00 <- data00[data00$sub != 8, ]
    data00$sub <- factor(data00$sub);
    cat(sprintf("Dropped %0.0f of %0.0f subject(s)\n",
                sub.pre - length(unique(data00$sub)), sub.pre))

### remove practice blocks, bad keypresses, and RTs <= 0
    data00 <- data00[data00$prac == 0, ]
    trials.pre <- nrow(data00)
    cat(sprintf("Starting with %0.0f experimental trials\n", trials.pre))
    data00 <- data00[data00$acc >= 0, ]
    cat(sprintf("Dropped %0.0f trials(s) for invalid responses\n",
                trials.pre - nrow(data00)))
    trials.pre <- nrow(data00)
    data00 <- data00[data00$rt > 50 & data00$rt < 5000, ]
    cat(sprintf("Dropped %0.0f trials(s) for being too fast/slow\n",
                trials.pre - nrow(data00)))
    cat(sprintf("%0.0f experimental trials remaining\n", nrow(data00)))
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
