### data04: compute median RT from no-gap trials separated by subject

f.data04 <- function () {
    infile <- "rawdata.rda"
    outfile <- "data04.rda"
    thisfile <- "data04.r"

    if (!file.exists(infile)) stop("cannot open file ", infile)
    if (IsFileUpToDate(outfile, c(thisfile, infile))) {
        warning("Output file is up to date, no action taken")
        return(invisible(NULL))
    }
    load(infile)
    dt <- alldata

    sub.pre <- length(unique(dt$sub))
### remove bad subjects:
    sub.pre <- length(unique(dt$sub))
    ## Subject 1 had 322 early responses out of 576 trials
    dt <- dt[dt$sub != 1, ]
    ## Subject 12 had 95
    dt <- dt[dt$sub != 12, ]
    ## Subject 7 had overall accuracy around 79%
    ##dt <- dt[dt$sub != 7, ]
    ## Subject 14 ran out of time and couldn't finish
    dt <- dt[dt$sub != 14, ]
    cat(sprintf("Dropped %0.0f of %0.0f subject(s)\n",
                sub.pre - length(unique(dt$sub)), sub.pre))

### remove trials with bad keypresses, too fast/slow RTs, and all those
### with a gap; also remove extra trial run before subject 10 actually
### started
    dt <- dt[dt$gapdur == 0, ]
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
