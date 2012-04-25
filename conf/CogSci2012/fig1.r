### fig1.r: Generate plot of RT by probe delay based on ProbeTrack15 for
### Navdeep's poster for the 2012 UC Berkeley CogSci Conference

f.fig1 <- function () {
    infile <- "../../probetrack15/data10.rda"
    outfile <- "fig1.pdf"
    exit.function <- function () {
        if (exists("opar")) par(opar)
        if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off()
    }
    on.exit(exit.function())
    load(infile)
    data10$soa <- as.numeric(as.character(data10$soa))
    data10$cond <- factor(as.character(data10$cond),
                        c("Blank", "BigFlash", "SmallFlash"))
    print(summary(data10))

    ## extract relevant data
    dt <- with(data10, tapply(rt, list(soa, cond), mean))
    x <- as.numeric(dimnames(dt)[[1]])
    col <- rainbow(length(dimnames(dt)[[2]]))

    ## CIs based on MSE of soa main effect for each cond
    ## order is Blank, SmallFlash, BigFlash
    errg <- sqrt(c(772.8, 366.2, 396.1) / 11) * qt(.975, 30)

    pdf(outfile, width=8, height=6, pointsize=16)
    opar <- par(mfrow=c(1, 1), las=1, pty="m", cex.axis=.6,
                xpd=NA, bg="white", mar=c(4.0, 4.0, 0.25, 3.0))

    matplot(x, dt, type="n", bty="n",
            ylim=c(360, 480), axes=F,
            xlab="Probe delay (ms)",
            ylab="Correct median RT (ms)",
            main="")
    axis(1, x)
    axis(2)

    for (i in 1:length(col)) {
        arrows(x, dt[, i] - errg[i], x, dt[, i] + errg[i],
               length=.05, angle=90, code=3, col=col[i], lwd=2, lty=1)
    }
    matlines(x, dt, type="o",
          col=col, pch=21, lty=1, lwd=4, cex=1.5, bg="white")
    legend("bottomright",
           c("Disappearance", "Brighter Background", "Darker Background"),
           bty="n", cex=.75, pt.cex=1.2,
           pt.bg="white", col=col, pch=21, lty=1, lwd=2)
}

f.fig1()
rm(f.fig1)
