### fig0301.r: plot average correct RT by probe delay and gap duration

f.fig0301 <- function () {
    infile <- "data03.rda"
    outfile <- "fig0301.pdf"
    exit.function <- function () {
        if (exists("opar")) par(opar)
        if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off()
    }
    on.exit(exit.function())

    load(infile)
    data03$soa <- as.numeric(as.character(data03$soa))

    ## extract relevant data
    dt <- with(data03, tapply(rt, list(soa, gap), mean))
    x <- as.numeric(dimnames(dt)[[1]])

    ## CIs based on MSE of soa main effect
    errg <- sqrt(c(223.14, 173.77) / 9) * qt(.975, 24)
    names(errg) <- c("Gap", "NoGap")

    pdf(outfile, width=6, height=6, pointsize=12)
    opar <- par(mfrow=c(1, 1), las=1, pty="m", cex.axis=.6,
                xpd=NA, bg="white")

    col <- c("navy", "orange")
    names(col) <- dimnames(dt)[[2]]

    matplot(x, dt, type="n", bty="n",
            ylim=c(450, 550), axes=F,
            xlab="Probe delay (ms)",
            ylab="Correct RT (ms)",
            main="ProbeTrack12")
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

f.fig0301()
rm(f.fig0301)
