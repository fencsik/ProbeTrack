### pb3a.r: generates plot for ProbeTrack03 results, separating out by gap
### duration
###
### $LastChangedDate$

f.pb3a <- function () {
    rtfile <- "../../probetrack03/data11.rda";
    dfile <- "../../probetrack03/data02.rda";
    outfile <- "pb3a.pdf";
    thisfile <- "pb3a.r";
    exit.function <- function () {
        if (exists("opar")) par(opar);
        if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off();
    }
    on.exit(exit.function());

    ## hard code error values for RT and d' (SOA main effect)
    err.rt <- sqrt(1385 / 8) * qt(.975, 49);
    err.dp <- sqrt(0.1506 / 8) * qt(.975, 49);

    ## hard code error values for RT and d' (GapDur main effect)
    ##    err.rt <- sqrt(36315 / 8) * qt(.975, 14);
    ##    err.dp <- sqrt(0.633 / 8) * qt(.975, 14);

    ## plotting limits
    ylim.rt <- c(550, 750);
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
    data.rt <- data11$data;
    fit.rt <- data11$fit;
    model.rt <- data11$model;
    load(dfile);
    data.dp <- data02;

    ## filter out unused trials
    data.dp <- data.dp[data.dp$gapdur != "0", ];

    ## extract data to plot
    rt <- with(data.rt, tapply(rt, list(soa, gapdur), mean));
    dp <- (with(data.dp, tapply(dprime, list(soa, gapdur), mean)) - ylim.dp[1]) /
        diff(ylim.dp) * diff(ylim.rt) * p.ylim.dp + ylim.rt[1];
    showx <- as.numeric(dimnames(rt)[[1]]);
    plotx <- showx; plotx[plotx == 960] <- 500; plotx[plotx == 1280] <- 600;

###    ## compute model for RT data
###    Subjects <- sort(unique(as.character(data.rt$sub)));
###    predx <- seq(min(plotx), max(plotx), by = 1);
###    rt.pred <- array(dim = c(length(Subjects), length(predx)),
###                   dimnames = list(Subjects, predx));
###    rtime <- fit.rt$rtime; names(rtime) <- fit.rt$sub;
###    baseRT <- fit.rt$baseRT; names(baseRT) <- fit.rt$sub;
###    for (sub in Subjects) {
###       rt.pred[sub, ] <- model.rt(predx, rtime[sub], baseRT[sub]);
###    }
###    rt.pred <- apply(rt.pred, 2, mean);

    ## or just compute based on average parameters
    predx <- seq(min(plotx), max(plotx), by = 1);
    rt.pred <- array(NA, dim = c(length(predx), dim(rt)[[2]]),
                     dimnames = list(predx, dimnames(rt)[[2]]))
    rtimes <- c(44.27448, 39.98592, 56.31590)
    baseRTs <- c(619.0732, 628.3832, 613.5311)
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
            main = "Experiment 3");
    axis(1, plotx, showx);
    axis(2);
    axis(4, ploty.dp, showy.dp);
    mtext("d'", side = 4, line = 2, las = 0, at = at.ylab.dp);

    ## plot error bars and points
    ##print(plotx <- array(plotx, dim = c(length(plotx), 3)))
    matlines(predx, rt.pred, lwd = 3, col = col, lty = 1);
    arrows(plotx, rt - err.rt, plotx, rt + err.rt,
           length = .05, angle = 90, code = 3, lwd = 1, lty = 1,
           col = rep(col, rep(length(plotx), 3)));
    matpoints(plotx, rt, pch = 21, col = col, bg = "white", lwd = 3, cex = 1.5);
    arrows(plotx, dp - err.dp, plotx, dp + err.dp,
           length = .05, angle = 90, code = 3, lwd = 1, lty = 1,
           col = rep(col, rep(length(plotx), 3)));
    matlines(plotx, dp, type = "o",
             lwd = 3, lty = 2, col = col, pch = 15, cex = 1.5);

    ## add breaks to x-axis
    if (require("plotrix")) {
        axis.break(axis = 1, breakpos = mean(c(320, 500)), style = "slash", brw = 0.02);
        axis.break(axis = 1, breakpos = mean(c(500, 600)), style = "slash", brw = 0.02);
    }

    legend("topright", paste(dimnames(rt)[[2]], "-ms gap", sep=""),
           bty = "n",
           col = col, lty = 1, lwd = 2)
}

f.pb3a();
rm(f.pb3a);
