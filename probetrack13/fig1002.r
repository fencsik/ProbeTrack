### fig1002.r: plot average median correct RT by probe delay and condition
### separately for each subject

f.fig1002 <- function () {
    infile <- "data10.rda"
    outfile <- "fig1002.pdf"
    thisfile <- "fig1002.r"
    exit.function <- function () {
        if (exists("opar")) par(opar)
        if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off()
    }
    on.exit(exit.function())

    load(infile)
    data10$sub <- as.numeric(as.character(data10$sub))
    data10$soa <- as.numeric(as.character(data10$soa))

    ## extract relevant data
    dt <- with(data10, tapply(rt, list(soa, cond, sub), mean,
                              na.rm=TRUE))
    x <- as.numeric(dimnames(dt)[[1]])
    col <- rainbow(length(dimnames(dt)[[2]]))

    pdf(outfile, width=9.5, height=7, pointsize=12)
    opar <- par(mfrow=c(2, 3), las=1, pty="m", cex.axis=.6,
                xpd=NA, bg="white")

    counter <- 0
    for (sub in dimnames(dt)[[3]]) {
        matplot(x, dt[, , sub], type="o", bty="n",
                ylim=c(300, 700), axes=F,
                col=col, pch=21, lty=1, lwd=3, bg="white",
                xlab="", ylab="", main=paste("ProbeTrack13 Sub", sub))
        axis(1, x)
        axis(2)
        if (counter %% 3 == 0) title(ylab="Correct median RT (ms)")
        if (counter %% 6 >= 3) title(xlab="Probe delay (ms)")
        if (counter %% 6 == 0) {
            legend("bottomright", dimnames(dt)[[2]],
                   inset=c(-1.35, -0.3), horiz=T,
                   bty="n", cex=.75, pt.cex=1.2,
                   pt.bg="white", col=col, pch=21, lty=1, lwd=2)
        }
        counter <- counter + 1
    }
}

f.fig1002()
rm(f.fig1002)
