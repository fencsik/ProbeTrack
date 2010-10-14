### data01: aggregate accuracy and RT statistics by subject, gap duration,
### probe type, number of targets, and probe delay
###
### $LastChangedDate$

f.data01 <- function () {
    infile <- "data00.rda"
    outfile <- "data01.rda"
    thisfile <- "data01.r"

    if (!file.exists(infile)) stop("cannot open file ", infile)
    if (IsFileUpToDate(outfile, c(thisfile, infile))) {
        warning("Output file is up to date, no action taken")
        return(invisible(NULL))
    }
    load(infile)

### extract factors for all trials and for all correct trials
    dataCC <- data00[data00$acc == 1, ]
    factorsA <- with(data00, list(soa=soa, target=target, ntargets=ntargets,
                                  gap=gap, sub=sub))
    factorsC <- with(dataCC, list(soa=soa, target=target, ntargets=ntargets,
                                  gap=gap, sub=sub))

### collapse across the factors
    data01 <- aggregate(data.frame(nobs = data00$acc), factorsA, length)
    data01$ncor <- aggregate(data00$acc, factorsA, sum)$x
    data01$pcor <- data01$ncor / data01$nobs
    data01$rt.all <- aggregate(data00$rt, factorsA, mean)$x
    data01$rt <- aggregate(dataCC$rt, factorsC, mean)$x

    save(data01, file=outfile)
}

f.data01()
rm(f.data01)
