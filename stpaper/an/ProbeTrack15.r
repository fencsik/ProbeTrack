### ProbeTrack15.r: generates plot for ProbeTrack15 results

do.ProbeTrack15 <- function () {
    rtfile <- "../../probetrack15/data11.rda"
    rtfile.nogap <- "../../probetrack15/data04.rda"
    dfile <- "../../probetrack15/data02.rda"
    outfile <- "ProbeTrack15.pdf"

    on.exit(if (exists("opar")) par(opar))
    on.exit(if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off(),
            TRUE)

    Conditions <- c("Blank", "SmallFlash", "BigFlash")

    ## settings
    show.no.gap.rt <- FALSE

    ## hard code error values for RT and d'
    err.rt <- sqrt(c(712, 306.49, 309.4) / 13) * qt(.975, 36)
    err.dp <- sqrt(c(0.2330, 0.1263, 0.16976) / 13) * qt(.975, 36)
    names(err.rt) <- names(err.dp) <- Conditions

    ## plotting limits
    ylim.rt <- c(200, 500)
    ylim.dp <- c(1, 4)
    p.ylim.dp <- 1/3
    showy.dp <- seq(ylim.dp[1], ylim.dp[2], by=1)
    ploty.dp <- (showy.dp - ylim.dp[1]) / diff(ylim.dp) *
        diff(ylim.rt) * p.ylim.dp + ylim.rt[1]
    at.ylab.dp <- mean(ploty.dp)
    err.dp <- err.dp / diff(ylim.dp) * diff(ylim.rt) * p.ylim.dp

    ## plotting symbols
    pch <- c(21, 25, 24)
    names(pch) <- Conditions

    ## load data files and rename the datasets
    load(rtfile)
    data.rt <- data11$data
    fit.rt <- data11$fit
    model.rt <- data11$model
    load(dfile)
    data.dp <- data02

    ## recode vars and filter out unused trials
    data.rt$soa <- as.numeric(as.character(data.rt$soa))
    data.dp$soa <- as.numeric(as.character(data.dp$soa))

    ## load and extract no-gap trials, if needed
    if (show.no.gap.rt) {
        load(rtfile.nogap)
        data.rt.nogap <- mean(data04$rt)
    }

    ## extract data to plot and rescale d-prime
    rt <- with(data.rt, tapply(rt, list(soa, cond), mean))
    dp <- (with(data.dp, tapply(dprime, list(soa, cond), mean)) - ylim.dp[1]) /
        diff(ylim.dp) * diff(ylim.rt) * p.ylim.dp + ylim.rt[1]
    showx <- as.numeric(dimnames(rt)[[1]])
    plotx <- showx

    ## compute model for RT data
    Subjects <- levels(data.rt$sub)
    predx <- seq(min(plotx), max(plotx), by=1)
    rt.pred <- array(dim=c(length(Subjects), length(predx), length(Conditions)),
                     dimnames=list(Subjects, predx, Conditions))
    rtime <- with(fit.rt, tapply(rtime, list(sub, cond), mean))
    baseRT <- with(fit.rt, tapply(baseRT, list(sub, cond), mean))
    for (sub in Subjects) {
        for (cond in Conditions) {
            rt.pred[sub, , cond] <- model.rt(predx, rtime[sub, cond], baseRT[sub, cond])
        }
    }
    rt.pred <- apply(rt.pred, 2:3, mean)

    ## or just compute based on average parameters
    ##rt.pred <- model.rt(predx, 40.345929, 493.494198)

    ## open pdf file
    pdf(outfile, width=8, height=6, pointsize=12)
    opar <- par(mfrow=c(1, 1), las=1, pty="s", cex.axis=.6,
                mar=c(5, 4, 2, 4), xpd=NA, bg="white")

    ## prepare plotting area
    plot(plotx, rt[, Conditions[1]], type="n", axes=F,
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
    for (cond in Conditions) {
        if (cond == "Blank") {
            lines(predx, rt.pred[, cond], lwd=3, col=1)
        }
        arrows(plotx, rt[, cond] - err.rt[cond], plotx, rt[, cond] + err.rt[cond],
               length=.05, angle=90, code=3, lwd=2, col=1, lty=1)
        if (cond == "Blank") {
            points(plotx, rt[, cond], pch=pch[cond], col=1, bg="white", lwd=3, cex=1.5)
        } else {
            lines(plotx, rt[, cond], col=1, lwd=2, lty=2)
            points(plotx, rt[, cond], pch=pch[cond], bg="white", col=1, lwd=3, cex=1.5)
        }
        arrows(plotx, dp[, cond] - err.dp[cond], plotx, dp[, cond] + err.dp[cond],
               length=.05, angle=90, code=3, lwd=2, col=1, lty=1)
        lines(plotx, dp[, cond], type="o",
              lwd=2, lty=2, pch=pch[cond], bg="black", cex=1.25)
    }
    legend("topright", c("Blank", "Small Change", "Big Change"),
           bty="n", pt.lwd=2, pch=pch, y.intersp=1.1)
}

do.ProbeTrack15()
rm(do.ProbeTrack15)
