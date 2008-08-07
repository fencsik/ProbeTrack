### fig1102.r: plot observed and predicted RT as a function of probe delay,
### separated by tracking load, averaged across subjects.
###
### $LastChangedDate$

f.fig1102 <- function () {
    infile <- "data11.rda";
    outfile <- "fig1102.pdf";
    thisfile <- "fig1102.r";
    exit.function <- function () {
        if (exists("opar")) par(opar);
        if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off();
    }
    on.exit(exit.function());

    if (!file.exists(infile)) stop("cannot open input file ", infile);
    if (IsFileUpToDate(outfile, c(infile, thisfile))) {
        warning("Output file is up to date, no action taken");
        return(invisible(NULL));
    }
    load(infile);

    obse <- with(data11$data, tapply(obs, list(soa, ntargets), mean));
    x <- as.numeric(dimnames(obse)[[1]]);
    rtime.avg <- with(data11$parameters, tapply(rtime, list(ntargets), mean));
    baseRT.avg <- with(data11$parameters, tapply(baseRT, list(ntargets), mean));
    rtime.all <- with(data11$parameters, tapply(rtime, list(ntargets, sub), mean));
    baseRT.all <- with(data11$parameters, tapply(baseRT, list(ntargets, sub), mean));

    ## fit model to each subject in each condition
    x.fit <- 0:max(x);
    nx <- length(x.fit);
    pred.avg <- array(dim = c(nx, dim(rtime.avg)[1]),
                      dimnames = list(as.character(x.fit), dimnames(rtime.avg)[[1]]));
    pred.all <- array(dim = c(nx, dim(rtime.all)[1], dim(rtime.all)[2]),
                      dimnames = list(as.character(x.fit), dimnames(pred.avg)[[2]],
                        dimnames(rtime.all)[[2]]));
    for (nt in dimnames(pred.all)[[2]]) {
        pred.avg[, nt] <- data11$model(x.fit, rtime.avg[nt], baseRT.avg[nt]);
        for (sub in dimnames(pred.all)[[3]]) {
            pred.all[, nt, sub] <- data11$model(x.fit, rtime.all[nt, sub], baseRT.all[nt, sub]);
        }
    }
    pred.all <- apply(pred.all, 1:2, mean);
    baseRT.all <- apply(baseRT.all, 1, mean);
    print(baseRT.all);

    ## plot settings
    col <- rainbow(dim(obse)[2], v = .75);
    pch <- c(21, 18, 3, 4);
    lwd.all <- 1; lwd.avg <- 2;
    lty.all <- 3; lty.avg <- 2;

    pdf(outfile, width = 6, height = 6, pointsize = 12);
    opar <- par(mfrow = c(1, 1), las = 1, pty = "m", cex.axis = .6,
                xpd = F, bg = "white");

    ylim <- c(450, 650);

    matplot(x, obse, type = "p", bty = "n",
            axes = F, ylim = ylim,
            col = col, pch = pch,
            xlab = "Probe delay (ms)", ylab = "Probe RT (ms)", main = "ProbeTrack6b");
    axis(1, x);
    axis(2);
    abline(v = 0, col = "gray50", lty = 1);
    matlines(x.fit, pred.all, col = col, lwd = lwd.all, lty = lty.all);
    matlines(x.fit, pred.avg, col = col, lwd = lwd.avg, lty = lty.avg);
    ##abline(h = baseRT.all, lty = 2, lwd = 1, col = col);
    legend("bottomright", sprintf("%s target(s)", dimnames(obse)[[2]]),
           bty = "n", ncol = 2, cex = .8,
           col = col, pch = pch);
    legend("bottomleft", c("Across Subj", "Avg Pars"),
           bty = "n", cex = .8, inset = c(.05, 0),
           lwd = c(lwd.all, lwd.avg), lty = c(lty.all, lty.avg));
}

f.fig1102();
rm(f.fig1102);
