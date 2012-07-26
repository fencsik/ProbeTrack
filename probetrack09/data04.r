### data04: compute median RT from no-gap trials separated by subject

f.data04 <- function () {
    infile <- "rawdata.txt"
    outfile <- "data04.rda"
    thisfile <- "data04.r"

    if (!file.exists(infile)) stop("cannot open file ", infile)
    if (IsFileUpToDate(outfile, c(thisfile, infile))) {
        warning("Output file is up to date, no action taken")
        return(invisible(NULL))
    }
    dt <- read.delim(infile)

    sub.pre <- length(unique(dt$sub))
### remove bad subjects:
    ## 1. Subject 2, 3, 6, and 15 had accuracy less that 80% in at least
    ## one SOA condition
    ## 2. Subject 8 has unusually high false-alarm rates
    ## dt <- dt[dt$sub != 2 & dt$sub != 3 &
    ##          dt$sub != 6 & dt$sub != 15, ]
    ## dt <- dt[dt$sub != 8, ]
    dt$sub <- factor(dt$sub)
    cat(sprintf("Dropped %0.0f of %0.0f subject(s)\n",
                sub.pre - length(unique(dt$sub)), sub.pre))

### remove trials with bad keypresses, too fast/slow RTs, and all those
### with a gap; also remove the first 10 practice non-gap trials
    dt <- dt[dt$gapdur == 0 & dt$trial > 10, ]
    trials.pre <- nrow(dt)
    cat(sprintf("Starting with %0.0f experimental no-gap trials\n", trials.pre))
    dt <- dt[dt$acc >= 0, ]
    cat(sprintf("Dropped %0.0f trials(s) for invalid responses\n",
                trials.pre - nrow(dt)))
    trials.pre <- nrow(dt)
    dt <- dt[dt$rt > 50 & dt$rt < 5000, ]
    cat(sprintf("Dropped %0.0f trials(s) for being too fast/slow\n",
                trials.pre - nrow(dt)))
    cat(sprintf("%0.0f experimental no-gap trials remaining\n", nrow(dt)))
    dt$blocktype <- factor(dt$blocktype)
    dt$resp <- factor(dt$resp)

### rename and recode variables
    ## probeType == 1 -> target; probeType == 2 -> distractor
    dt$target <- factor(dt$probeTarget, levels=c(0, 1),
                            labels=c("distractor", "target"))

    ## clean-up soa and convert to factor
    dt$soa <- dt$soa * 1000 / 75 # convert to ms
    dt$soa <- factor(dt$soa, levels=sort(unique(dt$soa)))

    ## extract factors for all trials and all correct trials
    dt.cor <- dt[dt$acc == 1, ]
    factors <- with(dt, list(sub=sub))
    factors.cor <- with(dt.cor, list(sub=sub))

    ## collapse across factors
    data04 <- aggregate(data.frame(rt.all=dt$rt), factors, median)
    data04$rt <- aggregate(data.frame(rt=dt.cor$rt), factors.cor, median)$rt

    save(data04, file=outfile)
}

f.data04()
rm(f.data04)
