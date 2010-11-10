### data03: aggregate RT by subject, gap duration, and probe delay

f.data03 <- function () {
    infile <- "data00.rda"
    outfile <- "data03.rda"
    load(infile)

### extract factors for all trials and for all correct trials
    dataCC <- data00[data00$acc == 1, ]
    factorsA <- with(data00, list(soa=soa, gap=gap, sub=sub))
    factorsC <- with(dataCC, list(soa=soa, gap=gap, sub=sub))

### collapse across the factors
    data03 <- aggregate(data.frame(nobs = data00$acc), factorsA, length)
    data03$rt.all <- aggregate(data00$rt, factorsA, mean)$x
    data03$rt <- aggregate(dataCC$rt, factorsC, mean)$x

    save(data03, file=outfile)
}

f.data03()
rm(f.data03)
