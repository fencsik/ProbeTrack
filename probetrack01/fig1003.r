### fig1003.r: plot correct RT by probe delay separated by gap duration,
### separately for each subject
###
### $LastChangedDate$

do.fig1003 <- function () {
   infile <- "data10.rda";
   outfile <- "fig1003.pdf";
   thisfile <- "fig1003.r";
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
   data10$soa <- as.numeric(as.character(data10$soa));
   data10$gapdur <- as.numeric(as.character(data10$gapdur));

   ## extract relevant data
   dtg <- with(data10[data10$gapdur != "0", ],
               tapply(rt, list(soa, sub, gapdur, ntargets), mean, na.rm = TRUE));
   x <- as.numeric(dimnames(dtg)[[1]]);
   dtng <- with(data10[data10$gapdur == "0",], tapply(rt, list(sub, ntargets), mean));

   ## settings
   ylim.range <- 300;

   pdf(outfile, width = 18, height = 8, pointsize = 12);
   opar <- par(mfrow = c(2, 4), las = 1, pty = "m", cex.axis = .6,
               xpd = NA, bg = "white");

   counter <- 0;
   for (sub in dimnames(dtg)[[2]]) {
      for (gd in dimnames(dtg)[[3]]) {
         for (nt in dimnames(dtg)[[4]]) {
            ylim <- c((mid <- mean(dtg[, sub, gd, nt])) - ylim.range/2, mid + ylim.range/2);
            plot(x, dtg[, sub, gd, nt], type = "n", bty = "n",
                    ylim = ylim, axes = F,
                    xlab = "", ylab = "", main = sprintf("ProbeTrack1 %s (Gap %s, %s targets)", sub, gd, nt));
            axis(1, x);
            axis(2);
            if (counter %% 8 >= 3) title(xlab = "Probe delay (ms)");
            if (counter %% 4 == 0) title(ylab = "Probe RT (ms)");

            lines(x, rep(dtng[sub, nt], length(x)), type = "l",
                  col = 1, lty = 2, lwd = 3);
            lines(x, dtg[, sub, gd, nt], type = "o",
                  col = 1, pch = 21, lty = 1, lwd = 3, cex = 1.5, bg = "white");
            if (counter %% 8 == 1) {
               legend("bottomleft", c("gap", "no gap"), inset = c(0.9, -.25),
                      lty = 1:2, lwd = 2,
                      bty = "n", ncol = 2, y.intersp = 1.3);
            }
            counter <- counter + 1;
         }
      }
   }
}

do.fig1003();
rm(do.fig1003);
