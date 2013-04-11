### ProbeTrack09.r: generates plot for ProbeTrack09 results

do.ProbeTrack09 <- function () {
    rtfile <- "../../probetrack09/data11.rda"
    rtfile.nogap <- "../../probetrack09/data04.rda"
    dfile <- "../../probetrack09/data02.rda"
    outfile <- "ProbeTrack09.pdf"

    on.exit(if (exists("opar")) par(opar))
    on.exit(if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off(),
            TRUE)

    ## settings
    show.no.gap.rt <- FALSE

    ## hard code error values for RT and d'
    err.rt <- sqrt(420 / 15) * qt(.975, 42)
    err.dp <- sqrt(0.1860 / 15) * qt(.975, 42)

    ## plotting limits
    ylim.rt <- c(200, 600)
    ylim.dp <- c(0, 3)
    p.ylim.dp <- 1/3
    showy.dp <- seq(ylim.dp[1], ylim.dp[2], by=1)
    ploty.dp <- (showy.dp - ylim.dp[1]) / diff(ylim.dp) *
        diff(ylim.rt) * p.ylim.dp + ylim.rt[1]
    at.ylab.dp <- mean(ploty.dp)
    err.dp <- err.dp / diff(ylim.dp) * diff(ylim.rt) * p.ylim.dp

    ## load data files and rename the datasets
    load(rtfile)
    data.rt <- data11$data
    fit.rt <- data11$fit
    model.rt <- data11$model
    load(dfile)
    data.dp <- data02

    ## recode vars and filter out unused trials
    data.rt$soa <- as.numeric(as.character(data.rt$soa))
    data.rt <- data.rt[data.rt$gapdur != "0", , drop=TRUE]
    data.dp$soa <- as.numeric(as.character(data.dp$soa))
    data.dp <- data.dp[data.dp$gapdur != "0", , drop=TRUE]

    ## load and extract no-gap trials, if needed
    if (show.no.gap.rt) {
        load(rtfile.nogap)
        data.rt.nogap <- mean(data04$rt)
    }

    ## extract data to plot
    rt <- with(data.rt, tapply(rt, list(soa), mean))
    dp <- (with(data.dp, tapply(dprime, list(soa), mean)) - ylim.dp[1]) /
        diff(ylim.dp) * diff(ylim.rt) * p.ylim.dp + ylim.rt[1]
    showx <- as.numeric(dimnames(rt)[[1]])
    plotx <- showx
    plotx[plotx == 1280] <- 500

    ## compute model for RT data
    Subjects <- sort(unique(as.character(data.rt$sub)))
    predx <- seq(min(plotx), max(plotx), by=1)
    rt.pred <- array(dim=c(length(Subjects), length(predx)),
                     dimnames=list(Subjects, predx))
    rtime <- fit.rt$rtime
    baseRT <- fit.rt$baseRT
    names(baseRT) <- names(rtime) <- fit.rt$sub
    for (sub in Subjects) {
        rt.pred[sub, ] <- model.rt(predx, rtime[sub], baseRT[sub])
    }
    rt.pred <- apply(rt.pred, 2, mean)

    ## or just compute based on average parameters
    ##rt.pred <- model.rt(predx, 40.345929, 493.494198)

    ## open pdf file
    pdf(outfile, width=8, height=6, pointsize=12)
    opar <- par(mfrow=c(1, 1), las=1, pty="s", cex.axis=.6,
                mar=c(5, 4, 2, 4), xpd=NA, bg="white")

    ## prepare plotting area
    plot(plotx, rt, type="n", axes=F,
         ylim=ylim.rt,
         xlab="Probe delay (ms)", ylab="Averaged median RT (ms)",
         main="")
    axis(1, plotx, showx)
    axis(2)
    axis(4, ploty.dp, showy.dp)
    mtext("d'", side=4, line=2, las=0, at=at.ylab.dp)

    ## plot error bars and points
    if (show.no.gap.rt) {
        abline(h=data.rt.nogap, lty="12", lwd=3, xpd=F)
    }
    lines(predx, rt.pred, lwd=3, col=1)
    arrows(plotx, rt - err.rt, plotx, rt + err.rt,
           length=.05, angle=90, code=3, lwd=2, col=1, lty=1)
    points(plotx, rt, pch=21, col=1, bg="white", lwd=3, cex=1.5)
    arrows(plotx, dp - err.dp, plotx, dp + err.dp,
           length=.05, angle=90, code=3, lwd=2, col=1, lty=1)
    lines(plotx, dp, type="o",
          lwd=3, lty=2, pch=21, bg="black", cex=1.5)

    ## add breaks to x-axis
    if (require("plotrix")) {
        axis.break(axis=1, breakpos=mean(c(160, 500)),
                   style="slash", brw=0.02)
    }
}

do.ProbeTrack09()
rm(do.ProbeTrack09)