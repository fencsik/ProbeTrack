### data10: aggregate median correct RT by subject, gap duration, tracking load,
### and probe delay
###
### $LastChangedDate$

f.data10 <- function () {
    infile <- "data00.rda";
    outfile <- "data10.rda";
    thisfile <- "data10.r";

    if (!file.exists(infile)) stop("cannot open file ", infile);
    if (IsFileUpToDate(outfile, c(thisfile, infile))) {
        warning("Output file is up to date, no action taken");
        return(invisible(NULL));
    }
    load(infile);

    ## extract factors for all trials and for all correct trials
    dt2 <- data00[data00$acc == 1, ];
    factorsC <- with(dt2, list(soa=soa, ntargets=ntargets, gapdur=gapdur, sub=sub));

    ## collapse across the factors
    data10 <- aggregate(data.frame(rt = dt2$rt), factorsC, median);
    data10$nobs <- aggregate(data.frame(rt = dt2$rt), factorsC, length)$rt;

    save(data10, file=outfile)
    invisible(data10)
}

f.data10();
rm(f.data10);
