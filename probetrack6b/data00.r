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
    data00 <- read.csv(infile);

### remove bad subjects:
    ## (none so far)

### remove practice blocks, bad keypresses, and RTs <= 0
    data00 <- data00[data00$block == 2 & data00$badkey == 0 & data00$rt > 0, ];

### rename and recode variables
    ## probeType == 1 -> target; probeType == 2 -> distractor
    data00$target <- factor(data00$probeType, levels = c(1, 2), labels = c("target", "distractor"));
    ## clean-up soa and convert to factor
    data00$soa[data00$gapdur == 0] <- 0; # set SOA to 0 on no-gap trials
    data00$soa <- data00$soa * 1000 / 75; # convert to ms
    data00$soa <- factor(data00$soa, levels = sort(unique(data00$soa)));
    ## convert ntargets to factor
    data00$ntargets <- factor(data00$ntargets, levels = sort(unique(data00$ntargets)));
    ## let's use acc (1 if correct) instead of error (1 if error)
    data00$acc <- 1 - data00$error;

    save(data00, file=outfile)
}

f.data00();
rm(f.data00);
