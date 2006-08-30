### fig0101.r: plot correct RT by probe delay separated by probe type
###
### $LastChangedDate$

do.fig0101 <- function () {
   infile <- "data01.rda";
   outfile <- "fig0101.pdf";
   thisfile <- "fig0101.r";
   errfile <- "an0103.rda";
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
   data01$soa <- as.numeric(as.character(data01$soa));

   ## extract relevant data
   dtg <- with(data01[data01$gapdur != "0", ],
               tapply(rt.cor, list(soa, target), mean, na.rm = TRUE));
   if (file.exists(errfile)) {
      load(errfile);
      errg <- an0103[, , "ci"];
   } else {
      errg <- with(data01[data01$gapdur != "0", ],
                   tapply(rt.cor, list(soa, target),
                          function(x) qt(.975, length(x) - 1) * sqrt(var(x, na.rm = TRUE) / length(x))));
   }
   dtng <- with(data01[data01$gapdur == "0", ],
                tapply(rt.cor, list(target), mean, na.rm = T));

   x <- as.numeric(dimnames(dtg)[[1]]) * 1000 / 75;

   ## settings
   ylim <- c(500, 1000);
   cond.names <- dimnames(dtg)[[2]];
   nCond <- length(cond.names);
   col <- rainbow(nCond);                               names(col) <- cond.names;
   pch <- c(21, 22, 23, 24, 16, 15, 17, 18)[1:nCond];   names(pch) <- cond.names;

   pdf(outfile, width = 6, height = 6, pointsize = 12);
   opar <- par(mfrow = c(1, 1), las = 1, pty = "m", cex.axis = .6,
               xpd = NA, bg = "white");

   matplot(x, dtg, type = "n", bty = "n",
           ylim = ylim, axes = F,
           xlab = "Probe delay (ms)", ylab = "Probe RT (ms)", main = "ProbeTrack1");
   axis(1, x);
   axis(2);

   lastIndex <- dim(dtg)[1];
   for (targ in dimnames(dtg)[[2]]) {
      if (!is.null(errg)) {
         arrows(x, dtg[, targ] - errg[, targ], x, dtg[, targ] + errg[, targ],
                length = .05, angle = 90, code = 3, lwd = 1, col = col[targ], lty = 1);
      }
      lines(x, dtg[, targ], type = "o",
            col = col[targ], pch = pch[targ], lty = 1, lwd = 3, cex = 1.5, bg = "white");
      lines(x, rep(dtng[targ], length(x)), type = "l",
            col = col[targ], lty = 2, lwd = 3);
###       text(x[lastIndex] + xinch(.2), dtg[lastIndex, n, targ], sprintf("%s targets", n),
###            col = col[targ], cex = .7, adj = 0);
      legend(max(x) - xinch(2), max(ylim), c("probe distractor", "probe target"),
             col = col[as.character(0:1)], pch = pch[as.character(0:1)],
             lty = 1, lwd = 3, pt.bg = "white", pt.cex = 1.5,
             bty = "n", y.intersp = 1.3);
      legend(max(x) - xinch(3.5), max(ylim), c("gap", "no gap"),
             lty = 1:2, lwd = 3,
             bty = "n", y.intersp = 1.3);
   }
}

do.fig0101();
rm(do.fig0101);
