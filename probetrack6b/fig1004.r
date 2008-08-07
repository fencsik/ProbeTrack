### fig1004.r: plot correct median RT as a function of tracking load, separated
### by probe delay, separately for each subject
###
### $LastChangedDate$

f.fig1004 <- function () {
    infile <- "data10.rda";
    outfile <- "fig1004.pdf";
    thisfile <- "fig1004.r";
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
    data10$sub <- as.character(data10$sub);
    data10$soa <- as.numeric(as.character(data10$soa));
    data10$ntargets <- as.numeric(as.character(data10$ntargets));
    data10$gapdur <- as.numeric(as.character(data10$gapdur));
    data10 <- data10[data10$gapdur > 0, ];

    ## extract relevant data
    dt <- with(data10, tapply(rt, list(ntargets, soa, sub), mean, na.rm = TRUE));
    x <- as.numeric(dimnames(dt)[[1]]);

    ## settings
    ylim <- c(300, 800);
    col <- rainbow(dim(dt)[2]);
    pch <- c(21, 23, 3, 4);
    lty <- 1;
    lwd <- 2;

    pdf(outfile, width = 9.5, height = 7, pointsize = 12);
    opar <- par(mfrow = c(2, 2), las = 1, pty = "m", cex.axis = .6,
                xpd = NA, bg = "white");

    counter <- 0;
    for (sub in dimnames(dt)[[3]]) {
        matplot(x, dt[, , sub], type = "o", bty = "n",
                axes = F, #ylim = ylim,
                xlab = "", ylab = "", main = paste("ProbeTrack6b", sub),
                col = col, pch = pch, lty = lty, lwd = lwd, bg = "white");
        axis(1, x);
        axis(2);
        if (counter %% 2 == 0) title(ylab = "Probe RT (ms)");
        if (counter %% 4 >= 2) title(xlab = "Tracking load");

        if (counter %% 4 == 0) {
            legend("bottomleft", sprintf("%s ms     ", dimnames(dt)[[2]]),
                   inset = c(.65, -.45),
                   col = col, pch = pch,
                   lty = lty, lwd = 3, pt.bg = "white", pt.cex = 1.5,
                   bty = "n", ncol = dim(dt)[2], y.intersp = 1.3, cex = .8);
        }
        counter <- counter + 1;
    }
}

f.fig1004();
rm(f.fig1004);
