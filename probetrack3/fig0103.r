### fig0103.r: plot correct RT by probe delay separated by probe type,
### separately for each gap duration and subject
###
### $LastChangedDate$

do.fig0103 <- function () {
   infile <- "data01.rda";
   outfile <- "fig0103.pdf";
   thisfile <- "fig0103.r";
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
   data01$soa <- as.numeric(as.character(data01$soa));
   data01$gapdur <- as.numeric(as.character(data01$gapdur));

   ## extract relevant data
   dtg <- with(data01[data01$gapdur != "0", ],
               tapply(rt.cor, list(soa, target, sub, gapdur), mean, na.rm = TRUE));
   dtng <- with(data01[data01$gapdur == "0", ],
                tapply(rt.cor, list(target, sub), mean, na.rm = T));

   x <- as.numeric(dimnames(dtg)[[1]]) * 1000 / 75;

   ## settings
   ylim <- c(400, 1200);
   cond.names <- dimnames(dtg)[[2]];
   nCond <- length(cond.names);
   col <- rainbow(nCond);                               names(col) <- cond.names;
   pch <- c(21, 22, 23, 24, 16, 15, 17, 18)[1:nCond];   names(pch) <- cond.names;

   pdf(outfile, width = 5, height = 9.5, pointsize = 12);
   opar <- par(mfrow = c(3, 1), las = 1, pty = "m", cex.axis = .6,
               xpd = NA, bg = "white");

   counter <- 0;
   for (sub in dimnames(dtg)[[3]]) {
      for (gd in dimnames(dtg)[[4]]) {
         matplot(x, dtg[, , sub, gd], type = "n", bty = "n",
                 ylim = ylim, axes = F,
                 xlab = "", ylab = "", main = sprintf("ProbeTrack3 %s (Gap %s)", sub, gd));

         axis(1, x);
         axis(2);
         if (counter %% 3 == 2) title(xlab = "Probe delay (ms)");
         title(ylab = "Probe RT (ms)");

         for (targ in dimnames(dtg)[[2]]) {
            lines(x, dtg[, targ, sub, gd], type = "o",
                  col = col[targ], pch = pch[targ], lty = 1, lwd = 3, cex = 1.5, bg = "white");
            lines(x, rep(dtng[targ, sub], length(x)), type = "l",
                  col = col[targ], lty = 2, lwd = 3);
            if (counter %% 3 < 2) {
               legend(min(x) + xinch(2), min(ylim) - yinch(.4), c("gap", "no gap"),
                      lty = 1:2, lwd = 2,
                      bty = "n", y.intersp = 1.3);
               legend(min(x) + xinch(3), min(ylim) - yinch(.4), c("probe distractor", "probe target"),
                      col = col[as.character(0:1)], pch = pch[as.character(0:1)],
                      lty = 1, lwd = 2, pt.bg = "white", pt.cex = 1.5,
                      bty = "n", y.intersp = 1.3);
            }
         }
         counter <- counter + 1;
      }
   }
}

do.fig0103();
rm(do.fig0103);
