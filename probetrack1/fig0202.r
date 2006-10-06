### fig0202.r: plot d' as a function of probe delay, separated by gap duration,
### separately for each subject
###
### $LastChangedDate$

do.fig0202 <- function () {
   infile <- "data02.rda";
   outfile <- "fig0202.pdf";
   thisfile <- "fig0202.r";
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
   data02$soa <- as.numeric(as.character(data02$soa));
   data02$gapdur <- as.numeric(as.character(data02$gapdur));

   ## extract relevant data
   dtg <- with(data02[data02$gapdur != "0", ],
               tapply(dprime, list(soa, sub, gapdur, ntargets), mean, na.rm = TRUE));
   x <- as.numeric(dimnames(dtg)[[1]]);
   dtng <- with(data02[data02$gapdur == "0",], tapply(dprime, list(sub, ntargets), mean));

   ## settings
   ylim <- c(0, 5);

   pdf(outfile, width = 9.5, height = 7, pointsize = 12);
   opar <- par(mfrow = c(2, 2), las = 1, pty = "m", cex.axis = .6,
               xpd = NA, bg = "white");

   counter <- 0;
   for (sub in dimnames(dtg)[[2]]) {
      for (gd in dimnames(dtg)[[3]]) {
         for (nt in dimnames(dtg)[[4]]) {
            plot(x, dtg[, sub, gd, nt], type = "n", bty = "n",
                    ylim = ylim, axes = F,
                    xlab = "", ylab = "", main = sprintf("ProbeTrack1 %s (Gap %s, %s targets)", sub, gd, nt));
            axis(1, x, x * 1000 / 75);
            axis(2);
            if (counter %% 4 >= 2) title(xlab = "Probe delay (ms)");
            if (counter %% 2 == 0) title(ylab = "d'");

            lines(x, rep(dtng[sub, nt], length(x)), type = "l",
                  col = 1, lty = 2, lwd = 3);
            lines(x, dtg[, sub, gd, nt], type = "o",
                  col = 1, pch = 21, lty = 1, lwd = 3, cex = 1.5, bg = "white");
            legend("bottomright", c("gap", "no gap"),
                   lty = 1:2, lwd = 3,
                   bty = "n", y.intersp = 1.3);
            counter <- counter + 1;
         }
      }
   }
}

do.fig0202();
rm(do.fig0202);
