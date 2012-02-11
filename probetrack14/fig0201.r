### fig0201.r: plot d' by probe delay

f.fig0201 <- function () {
    infile <- "data02.rda"
    outfile <- "fig0201.pdf"
    thisfile <- "fig0201.r"
    exit.function <- function () {
        if (exists("opar")) par(opar)
        if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off()
    }
    on.exit(exit.function())

    load(infile)
    data02$soa <- as.numeric(as.character(data02$soa))

    ## extract relevant data
    dt <- with(data02, tapply(dprime, list(soa), mean))
    x <- as.numeric(dimnames(dt)[[1]])
    col <- "black"

    ## CIs based on MSE from SOA main effect
    errg <- sqrt(c(.14602) / 8) * qt(.975, 28)

    pdf(outfile, width=6, height=6, pointsize=12)
    opar <- par(mfrow=c(1, 1), las=1, pty="m", cex.axis=.6,
                xpd=NA, bg="white")

    matplot(x, dt, type="n", bty="n",
            ylim=c(0, 4), axes=F,
            xlab="Probe delay (ms)", ylab="d'", main="ProbeTrack14")
    axis(1, x)
    axis(2)

    arrows(x, dt - errg, x, dt + errg,
           length=.05, angle=90, code=3, col=col, lwd=2, lty=1)
    matlines(x, dt, type="o",
          col=col, pch=21, lty=1, lwd=2, cex=1.5, bg="white")
}

f.fig0201()
rm(f.fig0201)