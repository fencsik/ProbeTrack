### fig0203.r: plot d' as a function of probe delay with error bars based on a
### repeated measures anova
###
### $LastChangedDate$

do.fig0203 <- function () {
   infile <- "data02.rda";
   outfile <- "fig0203.pdf";
   thisfile <- "fig0203.r";
   errfile <- "an0202.rda";
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
   data02$soa <- as.numeric(as.character(data02$soa));

   ## extract relevant data
   dtg <- with(data02[data02$gapdur != "0", ],
               tapply(dprime, list(soa), mean, na.rm = TRUE));
   nsub <- length(unique(data02$sub));
   errg <- sqrt(0.1914 / nsub) * qt(.975, 28);
   x <- as.numeric(dimnames(dtg)[[1]])
   dtng <- rep(mean(data02[data02$gapdur == "0", "dprime"], na.rm = TRUE), length(x));

   ## settings
   ylim <- c(0, 4);

   pdf(outfile, width = 6, height = 6, pointsize = 12);
   opar <- par(mfrow = c(1, 1), las = 1, pty = "m", cex.axis = .6,
               xpd = NA, bg = "white");

   plot(x, dtg, type = "l", bty = "n",
        lwd = 3, lty = 1, cex = 1.5,
        ylim = ylim, axes = F,
        xlab = "Probe delay (ms)", ylab = "d'", main = "ProbeTrack1");
   axis(1, x);
   axis(2);

   lines(x, dtng, type = "l",
         col = 1, lty = 2, lwd = 3);
   if (!is.null(errg)) {
      arrows(x, dtg - errg, x, dtg + errg,
             length = .05, angle = 90, code = 3, lwd = 1, col = 1, lty = 1);
   }
   points(x, dtg,
          col = 1, pch = 21, lwd = 3, cex = 1.5, bg = "white");
   legend("bottomright", c("gap", "no gap"),
          lty = 1:2, lwd = 3,
          bty = "n", y.intersp = 1.3);
}

do.fig0203();
rm(do.fig0203);
