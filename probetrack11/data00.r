### data00: cleans up raw data file

f.data00 <- function () {
    infile <- "rawdata.rda"
    outfile <- "data00.rda"
    load(infile)
    data00 <- alldata

### remove bad subjects:
    ## Overall accuracy < 80%
    data00 <- data00[data00$sub != 3, ]

### remove practice blocks, bad keypresses, and RTs <= 0
    data00 <- data00[data00$prac == 0 & data00$acc >= 0 & data00$rt > 0, ]
    data00$blocktype <- factor(data00$blocktype)
    data00$resp <- factor(data00$resp)

### rename and recode variables
    ## convert subject variable to a factor
    data00$sub <- factor(data00$sub)
    ## probeType == 1 -> target; probeType == 2 -> distractor
    data00$target <- factor(data00$probeTarget, levels=c(0, 1),
                            labels=c("distractor", "target"))
    ## clean-up soa and convert to factor
    data00$soa <- round(data00$soa * 1000 / 75) # convert to ms
    data00$soa <- factor(data00$soa, levels=sort(unique(data00$soa)))
    ## convert ntargets to factor
    data00$ntargets <- factor(data00$ntargets,
                              levels=sort(unique(data00$ntargets)))
    ## code gap factor
    data00$gap <- factor(data00$gapdur, levels=c(0, 10),
                         labels=c("NoGap", "Gap"))

    save(data00, file=outfile)
}

f.data00()
rm(f.data00)
