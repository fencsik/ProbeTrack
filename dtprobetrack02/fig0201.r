### fig0201.r: plot d' by probe delay
###
### $LastChangedDate$

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

    ## CIs based on MSE from SOA main effect
    errg <- sqrt(.1730 / 11) * qt(.975, 30)
    ## CIs based on MSE from smallest pair-wise SOA comparison
    ##errg <- sqrt( / 8) * qt(.975, 21)
    ## CIs based on MSE of soa main effect
    ##errg <- sqrt(.3308 / 8) * qt(.975, 21)

    pdf(outfile, width=6, height=6, pointsize=12)
    opar <- par(mfrow=c(1, 1), las=1, pty="m", cex.axis=.6,
                xpd=NA, bg="white")

    matplot(x, dt, type="n", bty="n",
            ylim=c(0, 4), axes=F,
            xlab="Probe delay (ms)", ylab="d'", main="DT-ProbeTrack02")
    axis(1, x)
    axis(2)

    if (!is.null(errg)) {
        arrows(x, dt - errg, x, dt + errg,
               length=.05, angle=90, code=3, lwd=1, lty=1)
    }
    lines(x, dt, type="o",
          pch=21, lty=1, lwd=3, cex=1.5, bg="white")
}

f.fig0201()
rm(f.fig0201)
