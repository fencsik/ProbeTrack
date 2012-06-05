### data00: cleans up raw data file

f.data00 <- function () {
    infile <- "rawdata.rda"
    outfile <- "data00.rda"

    load(infile)
    data00 <- alldata

### remove bad subjects:
    sub.pre <- length(unique(data00$sub))
    ## Subject 1 had 322 early responses out of 576 trials
    data00 <- data00[data00$sub != 1, ]
    ## Subject 12 had 95
    data00 <- data00[data00$sub != 12, ]
    ## Subject 7 had overall accuracy around 79%
    ##data00 <- data00[data00$sub != 7, ]
    ## Subject 14 ran out of time and couldn't finish
    data00 <- data00[data00$sub != 14, ]
    cat(sprintf("Dropped %0.0f of %0.0f subject(s)\n",
                sub.pre - length(unique(data00$sub)), sub.pre))

### remove practice blocks, bad keypresses, and responses that are too slow
### or too fast; note that there are some extremely large RTs, which are
### actually just early responses that were mishandled by the code: they
### were set to the clock rather than the RT
    trials.pre <- sum(data00$prac == 0)
    data00 <- data00[data00$prac == 0 & data00$acc >= 0 &
                     data00$rt > 50 & data00$rt < 5000, ]
    data00$blocktype <- factor(data00$blocktype)
    data00$resp <- factor(data00$resp)
    cat(sprintf("Dropped %0.0f of %0.0f experimental trials\n",
                trials.pre - sum(data00$prac == 0), trials.pre))

### rename and recode variables
    ## probeType == 1 -> target; probeType == 2 -> distractor
    data00$target <- factor(data00$probeTarget, levels = c(0, 1),
                            labels = c("distractor", "target"))

    ## clean-up soa and convert to factor
    data00$soa <- round(data00$soa * 1000 / 75) # convert to ms
    data00$soa <- factor(data00$soa, levels = sort(unique(data00$soa)))

    ## clean up gap type
    data00$gaptype <- factor(as.character(data00$gaptype),
                             levels=c("Blank", "SmallFlash", "BigFlash"))

    save(data00, file=outfile)
}

f.data00()
rm(f.data00)
