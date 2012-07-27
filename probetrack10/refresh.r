### Refresh all analysis files in this directory

refresh <- function () {
    targetFiles <- c("gatherData.r",
                     "data00.r",
                     "data01.r",
                     "data02.r",
                     "tab0201.r",
                     "fig0201.r",
                     "fig0202.r",
                     "data04.r",
                     "data10.r",
                     "tab1001.r",
                     "fig1001.r",
                     "data11.r",
                     "tab1101.r",
                     "fig1101.r",
                     "fig1102.r")

    for (fname in targetFiles) {
        if (!file.exists(fname)) stop(paste("File", fname, "not found"))
        cat("Refreshing", fname, "\n")
        flush.console()
        source(fname)
    }
}

refresh()
rm(refresh)
