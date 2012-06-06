### ProbeTrack06b-1.r: generates plot for ProbeTrack6b (Experiment 4)
### results, separating out by tracking load
###
### $LastChangedDate$

f.ProbeTrack06b.1 <- function () {
    rtfile <- "../../probetrack06b/data11.rda";
    dfile <- "../../probetrack06b/data02.rda";
    outfile <- "ProbeTrack06b-1.pdf";
    thisfile <- "ProbeTrack06b-1.r";
    exit.function <- function () {
        if (exists("opar")) par(opar);
        if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off();
    }
    on.exit(exit.function());

    ## hard code error values for RT and d' (SOA main effect)
    err.rt <- sqrt(1233 / 8) * qt(.975, 21);
    err.dp <- sqrt(0.2323 / 8) * qt(.975, 21);

    ## hard code error values for RT and d' (Load main effect)
    ##    err.rt <- sqrt(5641 / 8) * qt(.975, 21);
    ##    err.dp <- sqrt(0.2768 / 8) * qt(.975, 21);

    ## plotting limits
    ylim.rt <- c(300, 800);
    ylim.dp <- c(1, 4);
    p.ylim.dp <- 1/3;
    showy.dp <- seq(ylim.dp[1], ylim.dp[2], by = 1);
    ploty.dp <- (showy.dp - ylim.dp[1]) / diff(ylim.dp) * diff(ylim.rt) * p.ylim.dp + ylim.rt[1];
    at.ylab.dp <- mean(ploty.dp);
    err.dp <- err.dp / diff(ylim.dp) * diff(ylim.rt) * p.ylim.dp;

    if (!file.exists(rtfile)) stop("cannot open input file ", rtfile);
    if (!file.exists(dfile)) stop("cannot open input file ", dfile);
    if (IsFileUpToDate(outfile, c(rtfile, dfile, thisfile))) {
        warning("Output file is up to date, no action taken");
        return(invisible(NULL));
    }

    ## load data files and rename the datasets
    load(rtfile);
    data.rt <- data11$data;
    model.rt <- data11$model;
    load(dfile);
    data.dp <- data02;

    ## filter out unused trials
    data.dp <- data.dp[data.dp$gapdur != "0", ];

    ## extract data to plot
    rt <- with(data.rt, tapply(obs, list(soa, ntargets), mean));
    dp <- (with(data.dp, tapply(dprime, list(soa, ntargets), mean)) - ylim.dp[1]) /
        diff(ylim.dp) * diff(ylim.rt) * p.ylim.dp + ylim.rt[1];
    showx <- as.numeric(dimnames(rt)[[1]]);
    plotx <- showx; plotx[plotx == 1280] <- 500;

    ## compute model for RT data
    predx <- seq(min(plotx), max(plotx), by = 1);
    rt.pred <- array(NA, dim = c(length(predx), dim(rt)[[2]]),
                     dimnames = list(predx, dimnames(rt)[[2]]))
    rtimes <- c(38.70862, 26.41682, 55.62707, 44.21890)
    baseRTs <- c(496.9362, 571.3957, 554.9987, 589.8345)
    names(rtimes) <- names(baseRTs) <- dimnames(rt)[[2]]
    for (gap in dimnames(rt)[[2]]) {
        rt.pred[, gap] <- model.rt(predx, rtimes[gap], baseRTs[gap])
    }

    ## define colors
    col <- rainbow(dim(rt)[2], v = .75)

    ## open pdf file
    pdf(outfile, width = 8, height = 6, pointsize = 12);
    opar <- par(mfrow = c(1, 1), las = 1, pty = "s", cex.axis = .6,
                mar = c(5, 4, 2, 4), xpd = NA, bg = "white");

    ## prepare plotting area
    matplot(plotx, rt, type = "n", axes = F,
            ylim = ylim.rt, xlab = "Probe delay (ms)", ylab = "Averaged median RT (ms)",
            main = "Experiment 4");
    axis(1, plotx, showx);
    axis(2);
    axis(4, ploty.dp, showy.dp);
    mtext("d'", side = 4, line = 2, las = 0, at = at.ylab.dp);

    ## plot error bars and points
    matlines(predx, rt.pred, lwd = 3, col = col, lty = 1);
    arrows(plotx, rt - err.rt, plotx, rt + err.rt,
           length = .05, angle = 90, code = 3, lwd = 1, lty = 1,
           col = rep(col, rep(length(plotx), dim(rt)[[2]])));
    matpoints(plotx, rt, pch = 21, col = col, bg = "white", lwd = 3, cex = 1.5);
    arrows(plotx, dp - err.dp, plotx, dp + err.dp,
           length = .05, angle = 90, code = 3, lwd = 1, lty = 1,
           col = rep(col, rep(length(plotx), dim(rt)[[2]])));
    matlines(plotx, dp, type = "o",
             lwd = 3, lty = 2, col = col, pch = 15, cex = 1.5);

    ## add breaks to x-axis
    if (require("plotrix")) {
        axis.break(axis = 1, breakpos = mean(c(80, 500)), style = "slash", brw = 0.02);
    }

    legend("topright", paste(dimnames(rt)[[2]], "-target(s)", sep=""),
           bty = "n",
           col = col, lty = 1, lwd = 2)
}

f.ProbeTrack06b.1();
rm(f.ProbeTrack06b.1);
