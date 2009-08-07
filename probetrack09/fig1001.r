### fig1001.r: plot average median correct RT by probe delay
###
### $LastChangedDate$

f.fig1001 <- function () {
    infile <- "data10.rda"
    outfile <- "fig1001.pdf"
    thisfile <- "fig1001.r"
    exit.function <- function () {
        if (exists("opar")) par(opar)
        if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off()
    }
    on.exit(exit.function())

    if (!file.exists(infile)) stop("cannot open input file ", infile)
    if (IsFileUpToDate(outfile, c(infile, thisfile))) {
        warning("Output file is up to date, no action taken")
        return(invisible(NULL))
    }
    load(infile)
    data10$soa <- as.numeric(as.character(data10$soa))

    ## extract relevant data
    dt <- with(data10, tapply(rt, list(soa), mean))
    x <- as.numeric(dimnames(dt)[[1]])

    ## CIs based on MSE of soa main effect
    errg <- sqrt(509.7 / 11) * qt(.975, 30)

    pdf(outfile, width=6, height=6, pointsize=12)
    opar <- par(mfrow=c(1, 1), las=1, pty="m", cex.axis=.6,
                xpd=NA, bg="white")

    matplot(x, dt, type="n", bty="n",
            ylim=c(450, 750), axes=F,
            xlab="Probe delay (ms)",
            ylab="Correct median RT (ms)",
            main="ProbeTrack09")
    axis(1, x)
    axis(2)

    if (!is.null(errg)) {
        arrows(x, dt - errg, x, dt + errg,
               length=.05, angle=90, code=3, lwd=1, lty=1)
    }
    lines(x, dt, type="o",
          pch=21, lty=1, lwd=3, cex=1.5, bg="white")
}

f.fig1001()
rm(f.fig1001)