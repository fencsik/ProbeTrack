### fig0304.r: plot correct RT by probe delay separated by gap duration,
### separately for each subject
###
### $LastChangedDate$

do.fig0304 <- function () {
   infile <- "data03.rda";
   outfile <- "fig0304.pdf";
   thisfile <- "fig0304.r";
   exit.function <- function () {
      if (exists("opar")) par(opar);
      if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off();
   }
   on.exit(exit.function());

   if (IsFileUpToDate(outfile, c(infile, thisfile))) {
      warning("Output file is up to date, no action taken");
      return(invisible(NULL));
   }
   load(infile);
   data03$soa <- as.numeric(as.character(data03$soa));
   data03$gapdur <- as.numeric(as.character(data03$gapdur));

   ## extract relevant data
   dtg <- with(data03[data03$gapdur != "0", ],
               tapply(pcor, list(soa, sub, gapdur, ntargets), mean, na.rm = TRUE));
   x <- as.numeric(dimnames(dtg)[[1]]);
   dtng <- with(data03[data03$gapdur == "0",], tapply(pcor, list(sub, ntargets), mean));

   ## settings
   ylim.range <- .4;

   pdf(outfile, width = 9.5, height = 7, pointsize = 12);
   opar <- par(mfrow = c(2, 2), las = 1, pty = "m", cex.axis = .6,
               xpd = NA, bg = "white");

   counter <- 0;
   for (sub in dimnames(dtg)[[2]]) {
      for (gd in dimnames(dtg)[[3]]) {
         for (nt in dimnames(dtg)[[4]]) {
            ylim <- c((mid <- mean(dtg[, sub, gd, nt])) - ylim.range/2, mid + ylim.range/2);
            plot(x, dtg[, sub, gd, nt], type = "n", bty = "n",
                    ylim = ylim, axes = F,
                    xlab = "", ylab = "", main = sprintf("ProbeTrack2 %s (Gap %s, %s targets)", sub, gd, nt));
            axis(1, x, x * 1000 / 75);
            axis(2);
            if (counter %% 4 >= 2) title(xlab = "Probe delay (ms)");
            if (counter %% 2 == 0) title(ylab = "Proportion Correct");

            lines(x, rep(dtng[sub, nt], length(x)), type = "l",
                  col = 1, lty = 2, lwd = 3);
            lines(x, dtg[, sub, gd, nt], type = "o",
                  col = 1, pch = 21, lty = 1, lwd = 3, cex = 1.5, bg = "white");
            if (counter %% 4 == 0) {
               legend(min(x) + xinch(3), min(ylim) - yinch(.6), c("gap", "no gap"),
                      lty = 1:2, lwd = 2,
                      bty = "n", ncol = 2, y.intersp = 1.3);
            }
            counter <- counter + 1;
         }
      }
   }
}

do.fig0304();
rm(do.fig0304);
