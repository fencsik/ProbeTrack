### fig0201.r: plot average median correct RT by probe delay and gap duration

f.fig0201 <- function () {
    infile <- "data02.rda"
    outfile <- "fig0201.pdf"
    exit.function <- function () {
        if (exists("opar")) par(opar)
        if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off()
    }
    on.exit(exit.function())

    load(infile)
    data02$soa <- as.numeric(as.character(data02$soa))

    ## extract relevant data
    dt <- with(data02, tapply(dprime, list(soa, gap), mean))
    x <- as.numeric(dimnames(dt)[[1]])

    ## CIs based on MSE of soa main effect
    errg <- sqrt(c(0.19678, 0.198073) / 12) * qt(.975, 36)
    names(errg) <- c("Gap", "NoGap")

    pdf(outfile, width=6, height=6, pointsize=12)
    opar <- par(mfrow=c(1, 1), las=1, pty="m", cex.axis=.6,
                xpd=NA, bg="white")

    col <- c("navy", "orange")
    names(col) <- dimnames(dt)[[2]]

    matplot(x, dt, type="n", bty="n",
            ylim=c(2, 4), axes=F,
            xlab="Probe delay (ms)",
            ylab="d'",
            main="ProbeTrack11")
    axis(1, x)
    axis(2)

    for (g in dimnames(dt)[[2]]) {
        arrows(x, dt[, g] - errg[g], x, dt[, g] + errg[g],
               length=.05, angle=90, code=3, lwd=2, lty=1, col=col[g])
        lines(x, dt[, g], type="o",
              col=col[g], pch=21, lty=1, lwd=3, cex=1.5, bg="white")
    }
    legend("bottomright", dimnames(dt)[[2]],
           bty="n", cex=.75,
           col=col, lty=1, lwd=3)
}

f.fig0201()
rm(f.fig0201)
