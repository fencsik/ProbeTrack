### fig1001.r: plot average median correct RT by probe delay

f.fig1001 <- function () {
    infile <- "data10.rda"
    outfile <- "fig1001.pdf"
    thisfile <- "fig1001.r"
    exit.function <- function () {
        if (exists("opar")) par(opar)
        if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off()
    }
    on.exit(exit.function())

    load(infile)
    data10$soa <- as.numeric(as.character(data10$soa))

    ## extract relevant data
    dt <- with(data10, tapply(rt, list(soa), mean))
    x <- as.numeric(dimnames(dt)[[1]])
    col <- "black"

    ## CIs based on MSE of SOA main effect
    errg <- sqrt(c(513.75) / 8) * qt(.975, 28)

    pdf(outfile, width=6, height=6, pointsize=12)
    opar <- par(mfrow=c(1, 1), las=1, pty="m", cex.axis=.6,
                xpd=NA, bg="white")

    matplot(x, dt, type="n", bty="n",
            ylim=c(400, 600), axes=F,
            xlab="Probe delay (ms)",
            ylab="Correct median RT (ms)",
            main="ProbeTrack14")
    axis(1, x)
    axis(2)

    arrows(x, dt - errg, x, dt + errg,
           length=.05, angle=90, code=3, col=col, lwd=2, lty=1)
    matlines(x, dt, type="o",
             col=col, pch=21, lty=1, lwd=2, cex=1.5, bg="white")
}

f.fig1001()
rm(f.fig1001)
