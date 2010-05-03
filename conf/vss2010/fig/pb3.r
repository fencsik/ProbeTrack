### pb3.r: generates plot for ProbeTrack03 results, separating out by gap
### duration

f.pb3 <- function () {
    rtfile <- "../../../probetrack03/data11.rda"
    dfile <- "../../../probetrack03/data02.rda"
    outfile <- "pb3.pdf"
    thisfile <- "pb3.r"
    exit.function <- function () {
        if (exists("opar")) par(opar)
        if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off()
    }
    on.exit(exit.function())

    source("common.r")
    scaling <- max(scaling, 400)

    ## hard code error values for RT and d' (SOA main effect)
    err.rt <- sqrt(1385 / 8) * qt(.975, 49)
    err.dp <- sqrt(0.1506 / 8) * qt(.975, 49)

    ## hard code error values for RT and d' (GapDur main effect)
    ##    err.rt <- sqrt(36315 / 8) * qt(.975, 14)
    ##    err.dp <- sqrt(0.633 / 8) * qt(.975, 14)

    ## plotting limits
    ylim.rt <- c(550, 750) # old
    ylim.rt <- c(300, 800) # old
    ylim.max <- 725
    ylim.rt <- c(ylim.max - y.axis.range, ylim.max);
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

    ## filter out unused trials
    data.dp <- data.dp[data.dp$gapdur != "0", ]

    ## extract data to plot
    rt <- with(data.rt, tapply(rt, list(soa, gapdur), mean))
    dp <- (with(data.dp, tapply(dprime, list(soa, gapdur), mean)) - ylim.dp[1]) /
        diff(ylim.dp) * diff(ylim.rt) * p.ylim.dp + ylim.rt[1]
    showx <- as.numeric(dimnames(rt)[[1]])
    plotx <- showx
    plotx[plotx == 960] <- scaling
    plotx[plotx == 1280] <- scaling + 100

    ## compute model for RT data
    Subjects <- sort(unique(as.character(data.rt$sub)))
    predx <- seq(min(plotx), max(plotx), by=1)
    rt.pred <- array(dim=c(length(predx), dim(rt)[2], length(Subjects)),
                     dimnames=list(predx, dimnames(rt)[[2]], Subjects))
    rtime <- with(fit.rt, tapply(rtime, list(sub, gapdur), mean))
    baseRT <- with(fit.rt, tapply(baseRT, list(sub, gapdur), mean))
    for (sub in Subjects) {
        for (gd in dimnames(rtime)[[2]]) {
            rt.pred[, gd, sub] <-
                model.rt(predx, rtime[sub, gd], baseRT[sub, gd])
        }
    }
    rt.pred <- apply(rt.pred, 1:2, mean)

    ## or just compute based on average parameters
    ## predx <- seq(min(plotx), max(plotx), by=1)
    ## rt.pred <- array(NA, dim=c(length(predx), dim(rt)[[2]]),
    ##                  dimnames=list(predx, dimnames(rt)[[2]]))
    ## rtimes <- c(44.27448, 39.98592, 56.31590)
    ## baseRTs <- c(619.0732, 628.3832, 613.5311)
    ## names(rtimes) <- names(baseRTs) <- dimnames(rt)[[2]]
    ## for (gap in dimnames(rt)[[2]]) {
    ##     rt.pred[, gap] <- model.rt(predx, rtimes[gap], baseRTs[gap])
    ## }

    ## define colors
    col <- rainbow(dim(rt)[2], v=.75)

    ## open pdf file
    pdf(outfile, width=plotSize[1], height=plotSize[2], pointsize=fontSize)
    opar <- par(mfrow=c(1, 1), las=1, pty=pty, cex.axis=cex.axis,
                mar=mar, xpd=xpd, bg="white")

    ## prepare plotting area
    matplot(plotx, rt, type="n", axes=F,
            ylim=ylim.rt, main="",
            xlab="Probe delay (ms)", ylab="Averaged median RT (ms)")
    axis(1, plotx, showx, lwd=lwd.axis, cex.axis=cex.axis)
    axis(2, seq(ylim.rt[1], ylim.rt[2], by=100),
         lwd=lwd.axis, cex.axis=cex.axis)
    axis(4, ploty.dp, showy.dp, lwd=lwd.axis, cex.axis=cex.axis)
    mtext("d'", side=4, line=2, las=0, at=at.ylab.dp)

    ## plot error bars and points
    ##print(plotx <- array(plotx, dim=c(length(plotx), 3)))
    matlines(predx, rt.pred, lwd=lwd.model, col=col, lty=1)
    arrows(plotx, rt - err.rt, plotx, rt + err.rt,
           length=.05, angle=90, code=3, lwd=lwd.ci, lty=1,
           col=rep(col, rep(length(plotx), 3)))
    matpoints(plotx, rt, pch=pch.rt, col=col, bg="white",
              lwd=lwd.pts, cex=cex.pts)
    arrows(plotx, dp - err.dp, plotx, dp + err.dp,
           length=.05, angle=90, code=3, lwd=lwd.ci.dp, lty=1,
           col=rep(col, rep(length(plotx), 3)))
    matlines(plotx, dp, type="o", bg="white",
             lwd=lwd.dp, lty=2, col=col, pch=pch.dp, cex=cex.pts.dp)

    ## add breaks to x-axis
    if (require("plotrix")) {
        axis.break(axis=1, breakpos=mean(c(320, scaling)),
                   style="slash", brw=0.02)
        axis.break(axis=1, breakpos=mean(c(scaling, scaling+100)),
                   style="slash", brw=0.02)
    }
}

f.pb3()
rm(f.pb3)
