### pb3b.r: generates plot for ProbeTrack3 results, collapsing across gap
### duration
###
### $LastChangedDate$

f.pb3b <- function () {
    rtfile <- "../../probetrack3/data12.rda";
    dfile <- "../../probetrack3/data02.rda";
    outfile <- "pb3b.pdf";
    thisfile <- "pb3b.r";
    exit.function <- function () {
        if (exists("opar")) par(opar);
        if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off();
    }
    on.exit(exit.function());

    ## hard code error values for RT and d'
    err.rt <- sqrt(461.5 / 8) * qt(.975, 49);
    err.dp <- sqrt(0.1506 / 8) * qt(.975, 49);

    ## plotting limits
    ylim.rt <- c(300, 800);
    ylim.dp <- c(0, 3);
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
    data.rt <- data12$data;
    fit.rt <- data12$fit;
    model.rt <- data12$model;
    load(dfile);
    data.dp <- data02;

    ## filter out unused trials
    data.dp <- data.dp[data.dp$gapdur != "0", , drop = TRUE];

    ## extract data to plot
    rt <- with(data.rt, tapply(rt, list(soa), mean));
    dp <- (with(data.dp, tapply(dprime, list(soa), mean)) - ylim.dp[1]) /
        diff(ylim.dp) * diff(ylim.rt) * p.ylim.dp + ylim.rt[1];
    showx <- as.numeric(dimnames(rt)[[1]]);
    plotx <- showx; plotx[plotx == 960] <- 500; plotx[plotx == 1280] <- 600;

    ## compute model for RT data
    predx <- seq(min(plotx), max(plotx), by = 1);
    rt.pred <- model.rt(predx, 43.93182, 618.8494);

    ## open pdf file
    pdf(outfile, width = 8, height = 6, pointsize = 12);
    opar <- par(mfrow = c(1, 1), las = 1, pty = "s", cex.axis = .6,
                mar = c(5, 4, 2, 4), xpd = NA, bg = "white");

    ## prepare plotting area
    plot(plotx, rt, type = "n", axes = F,
         ylim = ylim.rt, xlab = "Probe delay (ms)", ylab = "Averaged median RT (ms)");
    axis(1, plotx, showx);
    axis(2);
    axis(4, ploty.dp, showy.dp);
    mtext("d'", side = 4, line = 2, las = 0, at = at.ylab.dp);

    ## plot error bars and points
    lines(predx, rt.pred, lwd = 3, col = 1);
    arrows(plotx, rt - err.rt, plotx, rt + err.rt,
           length = .05, angle = 90, code = 3, lwd = 2, col = 1, lty = 1);
    points(plotx, rt, pch = 21, col = 1, bg = "white", lwd = 3, cex = 1.5);
    arrows(plotx, dp - err.dp, plotx, dp + err.dp,
           length = .05, angle = 90, code = 3, lwd = 2, col = 1, lty = 1);
    lines(plotx, dp, type = "o",
          lwd = 3, lty = 2, pch = 15, cex = 1.5);

    ## add breaks to x-axis
    if (require("plotrix")) {
        axis.break(axis = 1, breakpos = mean(c(320, 500)), style = "slash", brw = 0.02);
        axis.break(axis = 1, breakpos = mean(c(500, 600)), style = "slash", brw = 0.02);
    }
}

f.pb3b();
rm(f.pb3b);
