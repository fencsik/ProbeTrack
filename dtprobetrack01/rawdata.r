### Collect each subject's data files into an R binary data file

f.gatherData <- function () {
    datadir <- "data"
    outfile <- "rawdata.rda"
    thisfile <- "gatherData.r"
    filelist <- "gatherDataList.rda"

    infiles <- file.path(datadir, dir(datadir))

    varnames <- NULL
    varnamesFromFile <- NULL
    alldata <- NULL
    filesLoaded <- infiles

    for (f in infiles) {
        if (!file.exists(f)) stop("cannot find input file ", f)
        cat("Opening data file ", f, "...", sep = "")
        dt <- read.delim(f)

        if (is.null(varnames)) {
            varnames <- colnames(dt)
            varnamesFromFile <- f
        } else if (dim(dt)[2] != length(varnames) || !all(names(dt) == varnames)) {
            warning("column names in ", f, " do not match those in ", varnamesFromFile)
            dt <- dt[, varnames]
        }

        if (is.null(alldata)) {
            alldata <- dt
        } else {
            alldata <- rbind(alldata, dt)
        }
        cat("done\n")
    }

    save(alldata, file=outfile)
    save(filesLoaded, file = filelist)
}

f.gatherData()
rm(f.gatherData)
