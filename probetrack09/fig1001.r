### fig1001.r: plot average median correct RT by probe delay

f.fig1001 <- function () {
    infile <- "data10.rda"
    infile2 <- "../probetrack02/data10.rda"
    outfile <- "fig1001.pdf"
    thisfile <- "fig1001.r"
    exit.function <- function () {
        if (exists("opar")) par(opar)
        if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off()
    }
    on.exit(exit.function())

    if (!file.exists(infile)) stop("cannot open input file ", infile)
    if (!file.exists(infile2)) stop("cannot open input file ", infile2)
    if (IsFileUpToDate(outfile, c(infile, infile2, thisfile))) {
        warning("Output file is up to date, no action taken")
        return(invisible(NULL))
    }
    load(infile2)
    dt2 <- data10
    load(infile)
    data10$soa <- as.numeric(as.character(data10$soa))

    ## extract relevant data
    dt <- with(data10, tapply(rt, list(soa), mean))
    x <- as.numeric(dimnames(dt)[[1]])

    dt2 <- with(dt2[dt2$gapdur > 0, ], tapply(rt, list(soa), mean))
    x2 <- as.numeric(dimnames(dt2)[[1]])

    ## CIs based on MSE of soa main effect
    errg <- sqrt(528.8 / 10) * qt(.975, 27)
    errg2 <- sqrt(1423 / 8) * qt(.975, 35)

    pdf(outfile, width=6, height=6, pointsize=12)
    opar <- par(mfrow=c(1, 1), las=1, pty="m", cex.axis=.6,
                xpd=NA, bg="white")

    matplot(x, dt, type="n", bty="n",
            ylim=c(400, 800), axes=F,
            xlab="Probe delay (ms)",
            ylab="Correct median RT (ms)",
            main="ProbeTrack09")
    axis(1, x)
    axis(2)

    arrows(x2, dt2 - errg2, x2, dt2 + errg2,
           length=.05, angle=90, code=3, lwd=2, lty=1)
    lines(x2, dt2, type="o",
          pch=21, lty=2, lwd=2, cex=1.5, bg="white")
    arrows(x, dt - errg, x, dt + errg,
           length=.05, angle=90, code=3, lwd=2, lty=1)
    lines(x, dt, type="o",
          pch=21, lty=1, lwd=2, cex=1.5, bg="white")
    legend("bottomright", c("ProbeTrack02", "ProbeTrack09"),
           bty="n", cex=.75, 
           col="black", lty=c(2, 1), lwd=c(2, 2))
    
}

f.fig1001()
rm(f.fig1001)
