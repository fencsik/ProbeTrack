### data00: cleans up raw data file
###
### $LastChangedDate$

f.data00 <- function () {
    infile <- "rawdata.txt";
    outfile <- "data00.rda";
    thisfile <- "data00.r";

    if (!file.exists(infile)) stop("cannot open file ", infile);
    if (IsFileUpToDate(outfile, c(thisfile, infile))) {
        warning("Output file is up to date, no action taken");
        return(invisible(NULL));
    }
    data00 <- read.delim(infile);

### remove bad subjects:
    ## 1. subjects 3, 4, 12, 24, 26, 27, 28, and 33 had overall accuracies below 70%
    ## 2. subject 32 had an overall average RT greater than 1000 ms due to a physical disability
    data00 <- data00[data00$sub != 3 & data00$sub != 4 & data00$sub != 12 &
                     data00$sub != 24 & data00$sub != 26 & data00$sub != 27 &
                     data00$sub != 28 & data00$sub != 33 & data00$sub != 32,]

### remove practice blocks, bad keypresses, and RTs <= 0
    data00 <- data00[data00$prac == 0 & data00$acc >= 0 & data00$rt > 0, ];
    data00$blocktype <- factor(data00$blocktype);
    data00$resp <- factor(data00$resp);

### rename and recode variables
    ## probeType == 1 -> target; probeType == 2 -> distractor
    data00$target <- factor(data00$probeTarget, levels = c(0, 1), labels = c("distractor", "target"));

    ## clean-up soa and convert to factor
    data00$soa <- data00$soa * 1000 / 75; # convert to ms
    data00$soa <- factor(data00$soa, levels = sort(unique(data00$soa)));
    ## convert ntargets to factor
    data00$ntargets <- factor(data00$ntargets, levels = sort(unique(data00$ntargets)));

    save(data00, file=outfile)
}

f.data00();
rm(f.data00);
