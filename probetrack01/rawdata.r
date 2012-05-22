### Convert text data file to binary

f.rawdata <- function () {
    infile <- "rawdata.txt"
    outfile <- "rawdata.rda"
    alldata <- read.csv(infile)
    save(alldata, file=outfile)
}

f.rawdata()
rm(f.rawdata)
