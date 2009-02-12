### fig1003.r: plot correct medial RT as a function of tracking load, separated
### by probe delay
###
### $LastChangedDate$

f.fig1003 <- function () {
    infile <- "data10.rda";
    outfile <- "fig1003.pdf";
    thisfile <- "fig1003.r";
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
    data10$soa <- as.numeric(as.character(data10$soa));
    data10$ntargets <- as.numeric(as.character(data10$ntargets));
    data10$gapdur <- as.numeric(as.character(data10$gapdur));
    data10 <- data10[data10$gapdur > 0, ];

    ## extract relevant data
    dt <- with(data10, tapply(rt, list(ntargets, soa), mean, na.rm = TRUE));
    x <- as.numeric(dimnames(dt)[[1]]);

    ## CIs based on MSE from ntargets main effect
    ##errg <- sqrt(5641 / 8) * qt(.975, 21);
    ## CIs based on MSE from average of ntargets effects at each SOA
    errg <- sqrt(mean(c(3346, 2208, 1239, 1644)) / 8) * qt(.975, 21);
    ## CIs based on MSE of soa main effect
    ##errg <- sqrt(1233 / 8) * qt(.975, 21);

    ## settings
    ylim <- c(450, 700);
    col <- rainbow(dim(dt)[2]); names(col) <- dimnames(dt)[[2]];
    pch <- c(21, 23, 3, 4);
    lty <- 1;
    lwd <- 2;

    pdf(outfile, width = 6, height = 6, pointsize = 12);
    opar <- par(mfrow = c(1, 1), las = 1, pty = "m", cex.axis = .6,
                xpd = NA, bg = "white");

    matplot(x, dt, type = "o", bty = "n",
            ylim = ylim, axes = F,
            xlab = "Tracking load", ylab = "Probe RT (ms)", main = "ProbeTrack6b",
            col = col, pch = pch, lty = lty, lwd = lwd, bg = "white");
    axis(1, x);
    axis(2);
    if (!is.null(errg)) {
        for (soa in dimnames(dt)[[2]]) {
            arrows(x, dt[, soa] - errg, x, dt[, soa] + errg,
                   length = .05, angle = 90, code = 3, lwd = 1, col = col[soa], lty = 1);
        }
        matpoints(x, dt,
                  col = col, pch = pch, lty = lty, lwd = lwd, bg = "white");
    }
    legend("top", max(ylim), sprintf("%s ms", dimnames(dt)[[2]]),
           col = col, pch = pch, pt.lwd = lwd, pt.bg = "white", pt.cex = 1.5,
           bty = "n", ncol = 4, y.intersp = 1.3, cex = .8);
}

f.fig1003();
rm(f.fig1003);
