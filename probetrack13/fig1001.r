### fig1001.r: plot average median correct RT by probe delay and condition

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
    dt <- with(data10, tapply(rt, list(soa, cond), mean))
    x <- as.numeric(dimnames(dt)[[1]])
    col <- rainbow(length(dimnames(dt)[[2]]))

    ## CIs based on MSE of soa main effect for each cond
    ## order is Blank, SmallFlash, BigFlash
    errg <- sqrt(c(490.71, 271.85, 282.04) / 17) * qt(.975, 48)

    pdf(outfile, width=6, height=6, pointsize=12)
    opar <- par(mfrow=c(1, 1), las=1, pty="m", cex.axis=.6,
                xpd=NA, bg="white")

    matplot(x, dt, type="n", bty="n",
            ylim=c(400, 600), axes=F,
            xlab="Probe delay (ms)",
            ylab="Correct median RT (ms)",
            main="ProbeTrack13")
    axis(1, x)
    axis(2)

    for (i in 1:length(col)) {
        arrows(x, dt[, i] - errg[i], x, dt[, i] + errg[i],
               length=.05, angle=90, code=3, col=col[i], lwd=2, lty=1)
    }
    matlines(x, dt, type="o",
          col=col, pch=21, lty=1, lwd=2, cex=1.5, bg="white")
    legend("bottomright", dimnames(dt)[[2]],
           bty="n", cex=.75, pt.cex=1.2,
           pt.bg="white", col=col, pch=21, lty=1, lwd=2)
}

f.fig1001()
rm(f.fig1001)
