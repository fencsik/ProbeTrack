f.refresh <- function () {
    infiles <- c("pb1a.r", "pb1b.r", "pb1c.r", "pb2.r", "pb3.r")
    for (f in infiles) {
        source(f)
    }
}

f.refresh()
rm(f.refresh)
