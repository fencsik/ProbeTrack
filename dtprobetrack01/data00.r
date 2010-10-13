### data00: cleans up raw data file
###
### $LastChangedDate$

f.data00 <- function () {
    infile <- "rawdata.rda"
    outfile <- "data00.rda"
    thisfile <- "data00.r"

    load(infile)
    data00 <- alldata

### remove bad subjects:
    ## 1. Subject 1 left early due to poor performance
    data00 <- data00[data00$sub != 1, ]
    ## 2. Subjects before 8 tracked 3 targets, and later ones tracked 2
    data00 <- data00[data00$sub >= 8, ]
    ## 3. Eliminate subjects with d' less than 1
    ### data00 <- data00[data00$sub != 12 & data00$sub != 14 &
    ###                  data00$sub != 15 & data00$sub != 16 &
    ###                  data00$sub != 17 & data00$sub != 19 &
    ###                  data00$sub != 21 & data00$sub != 23 &
    ###                  data00$sub != 24, ] 

### remove practice blocks, bad keypresses, and RTs <= 0
    data00 <- data00[data00$prac == 0 & data00$acc >= 0 & data00$rt > 0, ]
    data00$blocktype <- factor(data00$blocktype)
    data00$resp <- factor(data00$resp)

### rename and recode variables
    ## probeType == 1 -> target; probeType == 2 -> distractor
    data00$target <- factor(data00$probeTarget, levels = c(0, 1),
                            labels = c("distractor", "target"))

    ## clean-up soa and convert to factor
    data00$soa <- round(data00$soa * 1000 / 75) # convert to ms
    data00$soa <- factor(data00$soa, levels = sort(unique(data00$soa)))
    ## convert ntargets to factor
    data00$ntargets <- factor(data00$ntargets,
                              levels = sort(unique(data00$ntargets)))

    save(data00, file=outfile)
}

f.data00()
rm(f.data00)
