### ProbeTrack03-1.r: generates plot for ProbeTrack03 results, separating
### out by gap duration

f.ProbeTrack03.1 <- function () {
    rtfile.gapdur <- "../../probetrack03/data11.rda"
    rtfile.nogap <- "../../probetrack03/data10.rda"
    rtfile.no.gapdur <- "../../probetrack03/data14.rda"
    dfile <- "../../probetrack03/data02.rda"
    outfile <- "ProbeTrack03-1.pdf"
    exit.function <- function () {
        if (exists("opar")) par(opar)
        if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off()
    }
    on.exit(exit.function())

    Conditions <- c("133", "307", "507")

    ## Settings
    separate.pred.by.gapdur <- F
    plot.avg.pars <- F
    show.no.gap.rt <- F

    ## hard code error values for RT and d' (SOA main effect)
    err.rt <- sqrt(1376 / 8) * qt(.975, 49)
    err.dp <- sqrt(0.1512 / 8) * qt(.975, 49)

    ## hard code error values for RT and d' (GapDur main effect)
    ##    err.rt <- sqrt(36107 / 8) * qt(.975, 14)
    ##    err.dp <- sqrt(0.65 / 8) * qt(.975, 14)

    ## plotting limits
    ylim.rt <- c(350, 750)
    ylim.dp <- c(0, 3)
    p.ylim.dp <- 1/3
    showy.dp <- seq(ylim.dp[1], ylim.dp[2], by=1)
    ploty.dp <- (showy.dp - ylim.dp[1]) / diff(ylim.dp) * diff(ylim.rt) * p.ylim.dp + ylim.rt[1]
    at.ylab.dp <- mean(ploty.dp)
    err.dp <- err.dp / diff(ylim.dp) * diff(ylim.rt) * p.ylim.dp

    ## load data files and rename the datasets
    load(rtfile.gapdur)
    data.rt <- data11$data
    if (separate.pred.by.gapdur) {
        fit.rt <- data11$fit
    } else {
        load(rtfile.no.gapdur)
        fit.rt <- data14$fit
    }
    model.rt <- data11$model
    load(dfile)
    data.dp <- data02

    ## filter out unused trials
    data.dp <- data.dp[data.dp$gapdur != "0", ]

    ## load and extract no-gap trials, if needed
    if (show.no.gap.rt) {
        load(rtfile.nogap)
        data.rt.nogap <- mean(data10[data10$gapdur == 0, "rt"])
    }

    ## extract data to plot
    rt <- with(data.rt, tapply(rt, list(soa, gapdur), mean))
    dp <- (with(data.dp, tapply(dprime, list(soa, gapdur), mean)) - ylim.dp[1]) /
        diff(ylim.dp) * diff(ylim.rt) * p.ylim.dp + ylim.rt[1]
    showx <- as.numeric(dimnames(rt)[[1]])
    plotx <- showx
    plotx[plotx == 960] <- 500
    plotx[plotx == 1280] <- 600

    ## compute predicted RT
    predx <- seq(min(plotx), max(plotx), by=1)
    if (plot.avg.pars) {
        ## compute based on averaged parameters
        if (separate.pred.by.gapdur) {
            rt.pred <- array(dim=c(length(predx), length(Conditions)),
                             dimnames=list(predx, Conditions))
            rtimes <- apply(with(fit.rt, tapply(rtime, list(sub, gapdur), mean)), 2, mean)
            baseRTs <- apply(with(fit.rt, tapply(baseRT, list(sub, gapdur), mean)), 2, mean)
            names(rtimes) <- names(baseRTs) <- dimnames(rt)[[2]]
            for (gap in dimnames(rt)[[2]]) {
                rt.pred[, gap] <- model.rt(predx, rtimes[gap], baseRTs[gap])
            }
        } else {
            rtimes <- mean(fit.rt$rtime)
            baseRTs <- mean(fit.rt$baseRT)
            rt.pred <- model.rt(predx, rtimes, baseRTs)
            names(rt.pred) <- predx
        }
    } else {
        if (separate.pred.by.gapdur) {
            Subjects <- sort(unique(as.character(data.rt$sub)))
            rt.pred <- array(dim=c(length(Subjects), length(predx), length(Conditions)),
                             dimnames=list(Subjects, predx, Conditions))
            rtime <- with(fit.rt, tapply(rtime, list(sub, gapdur), mean))
            baseRT <- with(fit.rt, tapply(baseRT, list(sub, gapdur), mean))
            for (sub in Subjects) {
                for (cond in Conditions) {
                    rt.pred[sub, , cond] <- model.rt(predx, rtime[sub, cond], baseRT[sub, cond])
                }
            }
            rt.pred <- apply(rt.pred, 2:3, mean)
        } else {
            Subjects <- sort(unique(as.character(data.rt$sub)))
            rt.pred <- array(dim=c(length(Subjects), length(predx)),
                             dimnames=list(Subjects, predx))
            for (sub in Subjects) {
                rt.pred[sub, ] <- model.rt(predx,
                                           fit.rt$rtime[fit.rt$sub == sub],
                                           fit.rt$baseRT[fit.rt$sub == sub])
            }
            rt.pred <- apply(rt.pred, 2, mean)
        }
    }

    ## define plotting symbols and line types
    pch <- c(21, 22, 23)
    lty <- c("solid", "54", "12")

    ## open pdf file
    pdf(outfile, width=8, height=6, pointsize=12)
    opar <- par(mfrow=c(1, 1), las=1, pty="s", cex.axis=.6,
                mar=c(5, 4, 2, 4), xpd=NA, bg="white")

    ## prepare plotting area
    matplot(plotx, rt, type="n", axes=F,
            ylim=ylim.rt, xlab="Probe delay (ms)", ylab="Averaged median RT (ms)",
            main="")
    axis(1, plotx, showx)
    axis(2, seq(min(ylim.rt), max(ylim.rt), by=100))
    axis(4, ploty.dp, showy.dp)
    mtext("d'", side=4, line=2, las=0, at=at.ylab.dp)

    ## plot error bars and points
    if (show.no.gap.rt) {
        abline(h=data.rt.nogap, lty="12", lwd=3, xpd=F)
    }
    matlines(predx, rt.pred, lwd=3, col=1, lty=lty)
    arrows(plotx, rt - err.rt, plotx, rt + err.rt,
           length=.05, angle=90, code=3, lwd=1, lty=1,
           col=1)
    matpoints(plotx, rt, pch=pch, col=1, bg="white", lwd=3, cex=1.5)
    arrows(plotx, dp - err.dp, plotx, dp + err.dp,
           length=.05, angle=90, code=3, lwd=1, lty=1,
           col=1)
    matlines(plotx, dp, type="o",
             lwd=2, lty=2, col=1, pch=pch, bg="black", cex=1.25)

    ## add breaks to x-axis
    if (require("plotrix")) {
        axis.break(axis=1, breakpos=mean(c(320, 500)), style="slash", brw=0.02)
        axis.break(axis=1, breakpos=mean(c(500, 600)), style="slash", brw=0.02)
    }

    if (separate.pred.by.gapdur) {
        legend("topright", paste(dimnames(rt)[[2]], "-ms gap", sep=""),
               bty="n", y.intersp=1.2, col=1, lty=lty, lwd=3,
               pch=pch, pt.lwd=2, pt.cex=1.25, pt.bg="white")
    } else {
        legend("topright", paste(dimnames(rt)[[2]], "-ms gap", sep=""),
               bty="n", y.intersp=1.2, col=1,
               pch=pch, pt.lwd=2, pt.cex=1.25, pt.bg="white")
    }
}

f.ProbeTrack03.1()
rm(f.ProbeTrack03.1)
