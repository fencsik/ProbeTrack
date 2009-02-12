### fig1002.r: plot correct RT by probe delay and gap duration
###
### $LastChangedDate$

do.fig1002 <- function () {
   infile <- "data10.rda";
   outfile <- "fig1002.pdf";
   thisfile <- "fig1002.r";
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
   data10$gapdur <- as.numeric(as.character(data10$gapdur));

   ## extract relevant data
   dtg <- with(data10[data10$gapdur != "0", ],
               tapply(rt, list(soa, gapdur), mean, na.rm = TRUE));
   x <- as.numeric(dimnames(dtg)[[1]]);
   gapdurList <- dimnames(dtg)[[2]];
   nsub <- length(unique(data10$sub));
   ci <- function(mse, nsub, df) sqrt(mse / nsub) * qt(.975, df);
   errg <- rep(ci(1385, 8, 49), 3); names(errg) <- gapdurList;

   ## settings
   ylim <- c(500, 800);
   col <- rainbow(length(gapdurList)); names(col) <- gapdurList;

   pdf(outfile, width = 6, height = 6, pointsize = 12);
   opar <- par(mfrow = c(1, 1), las = 1, pty = "m", cex.axis = .6,
               xpd = NA, bg = "white");

   matplot(x, dtg, type = "n", bty = "n",
           ylim = ylim, axes = F,
           xlab = "Probe delay (ms)", ylab = "Probe RT (ms)", main = "ProbeTrack1");
   axis(1, x);
   axis(2);

   for (gd in gapdurList) {
      if (!is.null(errg)) {
         arrows(x, dtg[, gd] - errg[gd], x, dtg[, gd] + errg[gd],
                length = .05, angle = 90, code = 3, lwd = 2, col = col[gd], lty = 1);
      }
      lines(x, dtg[, gd], type = "o",
            col = col[gd], pch = 21, lty = 1, lwd = 3, cex = 1.5, bg = "white");
   }
   text(mean(c(min(x), max(x))), ylim[1] + .75 * diff(ylim),
        "95% CIs reflect SOA effect");
   legend("topright", sprintf("%0.0f ms", as.numeric(gapdurList)),
          lty = 1, lwd = 2, col = col,
          bty = "n", y.intersp = 1.3);
}

do.fig1002();
rm(do.fig1002);
