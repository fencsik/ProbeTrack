### ProbeTrack06b-3.r: generates plot for ProbeTrack06b (Experiment 4)
### results, plotting RT and d' by tracking load, separated by probe delay

f.ProbeTrack06b.3 <- function () {
    rtfile <- "../../probetrack06b/data11.rda";
    dfile <- "../../probetrack06b/data02.rda";
    outfile <- "ProbeTrack06b-3.pdf";
    exit.function <- function () {
        if (exists("opar")) par(opar);
        if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off();
    }
    on.exit(exit.function());

    ## hard code error values for RT and d' (SOA main effect)
    err.rt <- sqrt(1233 / 8) * qt(.975, 21);
    err.dp <- sqrt(0.2323 / 8) * qt(.975, 21);

    ## hard code error values for RT and d' (Load main effect)
    err.rt <- sqrt(5641 / 8) * qt(.975, 21);
    err.dp <- sqrt(0.277 / 8) * qt(.975, 21);

    ## plotting limits
    ylim.rt <- c(300, 700);
    ylim.dp <- c(1, 4);
    p.ylim.dp <- 1/3;
    showy.dp <- seq(ylim.dp[1], ylim.dp[2], by = 1);
    ploty.dp <- (showy.dp - ylim.dp[1]) / diff(ylim.dp) * diff(ylim.rt) * p.ylim.dp + ylim.rt[1];
    at.ylab.dp <- mean(ploty.dp);
    err.dp <- err.dp / diff(ylim.dp) * diff(ylim.rt) * p.ylim.dp;

    ## load data files and rename the datasets
    load(rtfile);
    data.rt <- data11$data;
    model.rt <- data11$model
    load(dfile);
    data.dp <- data02;

    ## filter out unused trials
    data.dp <- data.dp[data.dp$gapdur != "0", ];

    ## extract data to plot
    rt <- with(data.rt, tapply(obs, list(ntargets, soa), mean));
    dp <- (with(data.dp, tapply(dprime, list(ntargets, soa), mean)) - ylim.dp[1]) /
        diff(ylim.dp) * diff(ylim.rt) * p.ylim.dp + ylim.rt[1];
    showx <- as.numeric(dimnames(rt)[[1]]);
    plotx <- showx; ##plotx[plotx == 1280] <- 500;

    ## compute model for RT data
    predx <- as.numeric(dimnames(rt)[[2]])
    rt.pred <- array(NA, dim = c(dim(rt)[1], length(predx)),
                     dimnames = list(dimnames(rt)[[1]], predx))
    rtimes <- c(38.70862, 26.41682, 55.62707, 44.21890)
    baseRTs <- c(496.9362, 571.3957, 554.9987, 589.8345)
    names(rtimes) <- names(baseRTs) <- dimnames(rt)[[1]]
    for (nt in dimnames(rt)[[1]]) {
        rt.pred[nt, ] <- model.rt(predx, rtimes[nt], baseRTs[nt])
    }

    ## define colors
    col <- rainbow(dim(rt)[2], v = .75)

    ## open pdf file
    pdf(outfile, width = 8, height = 6, pointsize = 12);
    opar <- par(mfrow = c(1, 1), las = 1, pty = "s", cex.axis = .6,
                mar = c(5, 4, 2, 4), xpd = NA, bg = "white");

    ## prepare plotting area
    matplot(plotx, rt, type = "n", axes = F,
            ylim = ylim.rt, xlab = "Tracking load (# of targets)",
            ylab = "Averaged median RT (ms)",
            main = "");
    axis(1, plotx, showx);
    axis(2);
    axis(4, ploty.dp, showy.dp);
    mtext("d'", side = 4, line = 2, las = 0, at = at.ylab.dp);

    ## plot error bars and points
    ##matlines(plotx, rt.pred, lwd = 3, col = col, lty = 1);
    arrows(plotx, rt - err.rt, plotx, rt + err.rt,
           length = .05, angle = 90, code = 3, lwd = 1, lty = 1,
           col = rep(col, rep(length(plotx), dim(rt)[[2]])));
    matpoints(plotx, rt, type = "o",
              pch = 21, col = col, bg = "white", lwd = 3, lty = 1, cex = 1.5);
    arrows(plotx, dp - err.dp, plotx, dp + err.dp,
           length = .05, angle = 90, code = 3, lwd = 1, lty = 1,
           col = rep(col, rep(length(plotx), dim(rt)[[2]])));
    matlines(plotx, dp, type = "o",
             lwd = 3, lty = 2, col = col, pch = 15, cex = 1.5);

    legend("topleft", paste(dimnames(rt)[[2]], "-ms delay", sep=""),
           bty = "n",
           col = col, lty = 1, lwd = 2)
}

f.ProbeTrack06b.3();
rm(f.ProbeTrack06b.3);
