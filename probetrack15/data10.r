### data10: aggregate median correct RT by subject, condition, and probe
### delay

f.data10 <- function () {
    infile <- "data00.rda"
    outfile <- "data10.rda"
    thisfile <- "data10.r"

    load(infile)

    ## extract factors for all trials and for all correct trials
    dt2 <- data00[data00$acc == 1, ]
    factorsC <- with(dt2, list(soa=soa, cond=gaptype, sub=sub))

    ## collapse across the factors
    data10 <- aggregate(data.frame(rt = dt2$rt), factorsC, median)
    data10$nobs <- aggregate(data.frame(rt = dt2$rt), factorsC, length)$rt

    data10$sub <- factor(data10$sub)

    save(data10, file=outfile)
    invisible(data10)
}

f.data10()
rm(f.data10)
