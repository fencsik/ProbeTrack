### fig0301.r: plot correct RT by probe delay
###
### $LastChangedDate$

do.fig0301 <- function () {
   infile <- "data03.rda";
   outfile <- "fig0301.pdf";
   thisfile <- "fig0301.r";
   errfile <- "an0303.rda";
   exit.function <- function () {
      if (exists("opar")) par(opar);
      if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off();
   }
   on.exit(exit.function());

   if (!file.exists(infile)) stop("cannot open input file ", infile);
   if (IsFileUpToDate(outfile, c(infile, thisfile, errfile))) {
      warning("Output file is up to date, no action taken");
      return(invisible(NULL));
   }
   load(infile);
   data03$soa <- as.numeric(as.character(data03$soa));

   ## extract relevant data
   dtg <- with(data03[data03$gapdur != "0", ],
               tapply(rt, list(soa), mean, na.rm = TRUE));
   if (file.exists(errfile)) {
      load(errfile);
      errg <- an0303[, "ci"];
   } else {
      errg <- with(data03[data03$gapdur != "0", ],
                   tapply(rt, list(soa),
                          function(x) qt(.975, length(x) - 1) * sqrt(var(x, na.rm = TRUE) / length(x))));
   }
   x <- as.numeric(dimnames(dtg)[[1]]);
   dtng <- rep(mean(data03[data03$gapdur == "0", "rt"], na.rm = F), length(x));

   ## settings
   ylim <- c(500, 1000);

   pdf(outfile, width = 6, height = 6, pointsize = 12);
   opar <- par(mfrow = c(1, 1), las = 1, pty = "m", cex.axis = .6,
               xpd = NA, bg = "white");

   plot(x, dtg, type = "n", bty = "n",
        ylim = ylim, axes = F,
        xlab = "Probe delay (ms)", ylab = "Probe RT (ms)", main = "ProbeTrack2");
   axis(1, x);
   axis(2);

   if (!is.null(errg)) {
      arrows(x, dtg - errg, x, dtg + errg,
             length = .05, angle = 90, code = 3, lwd = 1, col = 1, lty = 1);
   }
   lines(x, dtng, type = "l",
         col = 1, lty = 2, lwd = 3);
   lines(x, dtg, type = "o",
         col = 1, pch = 21, lty = 1, lwd = 3, cex = 1.5, bg = "white");
   legend("topright", c("gap", "no gap"),
          lty = 1:2, lwd = 3,
          bty = "n", y.intersp = 1.3);
}

do.fig0301();
rm(do.fig0301);
