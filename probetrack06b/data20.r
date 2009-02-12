### data20: aggregate median correct RT by subject, gap duration, tracking load,
### and probe delay for probe-target trials only
###
### $LastChangedDate$

f.data20 <- function () {
    infile <- "data00.rda";
    outfile <- "data20.rda";
    thisfile <- "data20.r";

    if (!file.exists(infile)) stop("cannot open file ", infile);
    if (IsFileUpToDate(outfile, c(thisfile, infile))) {
        warning("Output file is up to date, no action taken");
        return(invisible(NULL));
    }
    load(infile);

    ## eliminate probe-distractor trials
    data00 <- data00[data00$target == "target", ]
    data00$target <- factor(data00$target)

    ## extract factors for all trials and for all correct trials
    dt2 <- data00[data00$acc == 1, ];
    factorsC <- with(dt2, list(soa=soa, ntargets=ntargets, gapdur=gapdur, sub=sub));

    ## collapse across the factors
    data20 <- aggregate(data.frame(rt = dt2$rt), factorsC, median);
    data20$nobs <- aggregate(data.frame(rt = dt2$rt), factorsC, length)$rt;

    save(data20, file=outfile)
    invisible(data20)
}

f.data20();
rm(f.data20);
