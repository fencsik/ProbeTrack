### fig0104.r: plot accuracy by probe delay separated by gap duration and probe
### type, separately for each subject
###
### $LastChangedDate$

do.fig0104 <- function () {
   infile <- "data01.rda";
   outfile <- "fig0104.pdf";
   thisfile <- "fig0104.r";
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

   ## extract relevant data
   dtg <- with(data01[data01$gapdur != "0", ],
               tapply(pcor, list(soa, target, sub), mean, na.rm = TRUE));
   dtng <- with(data01[data01$gapdur == "0", ],
                tapply(pcor, list(target, sub), mean, na.rm = T));

   x <- as.numeric(dimnames(dtg)[[1]]);

   ## settings
   ylim <- c(.5, 1);
   cond.names <- dimnames(dtg)[[2]];
   nCond <- length(cond.names);
   col <- rainbow(nCond);                               names(col) <- cond.names;
   pch <- c(21, 22, 23, 24, 16, 15, 17, 18)[1:nCond];   names(pch) <- cond.names;

   pdf(outfile, width = 9.5, height = 7, pointsize = 12);
   opar <- par(mfrow = c(2, 2), las = 1, pty = "m", cex.axis = .6,
               xpd = NA, bg = "white");

   counter <- 0;
   for (sub in dimnames(dtg)[[3]]) {
      matplot(x, dtg[, , sub], type = "n", bty = "n",
              ylim = ylim, axes = F,
              xlab = "", ylab = "", main = sprintf("ProbeTrack1 %s", sub));
      axis(1, x);
      axis(2);
      if (counter %% 4 >= 2) {
         title(xlab = "Probe delay (ms)");
      }
      if (counter %% 2 == 0) {
         title(ylab = "Proportion correct");
      }

      for (targ in dimnames(dtg)[[2]]) {
         lines(x, dtg[, targ, sub], type = "o",
               col = col[targ], pch = pch[targ], lty = 1, lwd = 3, cex = 1.5, bg = "white");
         lines(x, rep(dtng[targ, sub], length(x)), type = "l",
               col = col[targ], lty = 2, lwd = 3);
         if (counter %% 4 == 0) {
            legend("bottomright", c("gap", "no gap"), inset = c(-.1, -.55),
                   lty = 1:2, lwd = 3,
                   bty = "n", y.intersp = 1.3);
            legend("bottomright", c("probe distractor", "probe target"), inset = c(-.6, -.55),
                   col = col[as.character(0:1)], pch = pch[as.character(0:1)],
                   lty = 1, lwd = 3, pt.bg = "white", pt.cex = 1.5,
                   bty = "n", y.intersp = 1.3);
         }
      }
      counter <- counter + 1;
   }
}

do.fig0104();
rm(do.fig0104);
