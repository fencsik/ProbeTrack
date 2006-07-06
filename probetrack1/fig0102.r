### fig0102.r: plot accuracy by probe delay separated by probe type
###
### $LastChangedDate$

do.fig0102 <- function () {
   infile <- "data01.rda";
   pdffile <- "fig0102.pdf";
   exit.function <- function () {
      if (exists("opar")) par(opar);
      if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off();
   }
   on.exit(exit.function());

   if (!file.exists(infile)) stop("cannot open input file ", infile);
   load(infile);
   data01$soa <- as.numeric(as.character(data01$soa));

   ## extract relevant data
   dtg <- with(data01[data01$gapdur != "0", ],
               tapply(pcor, list(soa, target), mean, na.rm = TRUE));
   errg <- with(data01[data01$gapdur != "0", ],
                tapply(pcor, list(soa, target),
                       function(x) sqrt(var(x, na.rm = TRUE) / length(x))));
   dtng <- with(data01[data01$gapdur == "0", ],
                tapply(pcor, list(soa, target), mean, na.rm = TRUE));

   x <- as.numeric(dimnames(dtg)[[1]]) * 1000 / 75;

   ## settings
   ylim <- c(.5, 1);
   cond.names <- dimnames(dtg)[[2]];
   nCond <- length(cond.names);
   col <- rainbow(nCond);                               names(col) <- cond.names;
   pch <- c(21, 22, 23, 24, 16, 15, 17, 18)[1:nCond];   names(pch) <- cond.names;

   pdf(pdffile, width = 6, height = 6, pointsize = 12);
   opar <- par(mfrow = c(1, 1), las = 1, pty = "m", cex.axis = .6,
               xpd = NA, bg = "white");

   matplot(x, dtg, type = "n", bty = "n",
           ylim = ylim, axes = F,
           xlab = "Probe delay (ms)", ylab = "Proportion of correct trials", main = "ProbeTrack1");
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
      lines(range(x), rep(mean(dtng[, targ]), 2), type = "l",
            col = col[targ], lty = 2, lwd = 3);
###       text(x[lastIndex] + xinch(.2), dtg[lastIndex, n, targ], sprintf("%s targets", n),
###            col = col[targ], cex = .7, adj = 0);
      legend(max(x) - xinch(2), max(ylim) + yinch(.25), c("probe distractor", "probe target"),
             col = col[as.character(0:1)], pch = pch[as.character(0:1)],
             lty = 1, lwd = 3, pt.bg = "white", pt.cex = 1.5,
             bty = "n", y.intersp = 1.3);
      legend(max(x) - xinch(3.5), max(ylim) + yinch(.25), c("gap", "no gap"),
             lty = 1:2, lwd = 3,
             bty = "n", y.intersp = 1.3);
   }
}

do.fig0102();
rm(do.fig0102);
