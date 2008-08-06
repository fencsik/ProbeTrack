### fig1101.r: plot observed and predicted RT as a function of probe delay,
### separated by tracking load, separately for each subject
###
### $LastChangedDate$

f.fig1101 <- function () {
    infile <- "data11.rda";
    outfile <- "fig1101.pdf";
    thisfile <- "fig1101.r";
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

    obse <- with(data11$data, tapply(obs, list(soa, ntargets, sub), mean));
    rtime <- with(data11$parameters, tapply(rtime, list(ntargets, sub), mean));
    baseRT <- with(data11$parameters, tapply(baseRT, list(ntargets, sub), mean));
    x <- as.numeric(dimnames(obse)[[1]]);
    x.fit <- 0:max(x);
    pred <- array(NA, dim = c(length(x.fit), dim(obse)[2]),
                  dimnames = list(as.character(x.fit), dimnames(obse)[[2]]));

    ## plot settings
    condNames <- dimnames(obse)[[2]];
    nCond <- length(condNames);
    col <- rainbow(nCond, v = .75); names(col) <- condNames;
    lwd <- c(1, 1);
    lty <- c(3, 2);
    ylim <- c(300, 1000);
    pt.bg <- rep("white", 2);
    pch <- c(21, 4);

    pdf(outfile, width = 8, height = 8, pointsize = 12);
    opar <- par(mfrow = c(2, 2), las = 1, pty = "m", cex.axis = .6,
                xpd = NA, bg = "white");

    counter <- 0;
    for (sub in dimnames(obse)[[3]]) {
        matplot(x, obse[, , sub], type = "p", bty = "n",
                axes = F, cex = .75, #ylim = ylim,
                col = col, lty = lty[1], lwd = lwd[1], pch = pch[1], bg = pt.bg,
                xlab = "", ylab = "", main = sprintf("ProbeTrack6 - %s", sub));
        for (nt in dimnames(obse)[[2]]) {
            pred[, nt] <- data11$model(x.fit, rtime[nt, sub], baseRT[nt, sub]);
        }
        matlines(x.fit, pred,
                 col = col, lty = lty[2], lwd = lwd[2], pch = pch[2]);
        ## matlines(x, pred[, , sub], type = "o",
        ##          col = col, lty = lty[2], lwd = lwd[2], pch = pch[2]);
        axis(1, x);
        axis(2);
        if (counter %% 4 > 1) title(xlab = "Probe delay (ms)");
        if (counter %% 2 == 0) title(ylab = "Probe RT (ms)");
        if (counter %% 4 == 0) {
            legend("bottomright", sprintf("%s targets", condNames), inset = c(-.4, -.45),
                   bty = "n", col = col, lwd = 2, ncol = 1);
        }
        counter <- counter + 1;
    }
}

f.fig1101();
rm(f.fig1101);
