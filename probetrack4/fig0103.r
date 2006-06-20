### fig0103.r: plot correct RT by probe delay separated by number of targets
###
### $LastChangedDate$

do.fig0103 <- function () {
   infile <- "data01.rda";
   pdffile <- "fig0103.pdf";
   exit.function <- function () {
      if (exists("opar")) par(opar);
      if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off();
   }
   on.exit(exit.function());

   if (!file.exists(infile)) stop("cannot open input file ", infile);
   load(infile);

   ## extract relevant data
   dt <- data01[data01$gapDuration > 0 & data01$acc == 1, ];
   dtg <- apply(tapply(dt$rt, list(dt$soa, dt$nTargets, dt$sub), mean),
                1:2, mean, na.rm = T);
   errg <- NULL;
   x <- as.numeric(dimnames(dtg)[[1]]) * 1000 / 75;

   ### settings
   ylim <- c(500, 1000);
   cond.names <- dimnames(dtg)[[2]];
   nCond <- length(cond.names);
   col <- rainbow(nCond);                               names(col) <- cond.names;
   col[as.character(1:4)] <- c("green3", "blue", "red", "black");
   pch <- c(21, 22, 23, 24, 16, 15, 17, 18)[1:nCond];   names(pch) <- cond.names;

   pdf(pdffile, width = 6, height = 6, pointsize = 12);
   opar <- par(mfrow = c(1, 1), las = 1, pty = "s", cex.axis = .6,
               xpd = NA, bg = "white");

   matplot(x, dtg, type = "n", bty = "n",
           ylim = ylim, axes = F,
           xlab = "Probe delay (ms)", ylab = "Reaction time (ms)", main = "ProbeTrack4/3B");
   axis(1, x);
   axis(2);

   lastIndex <- dim(dtg)[1];
   for (n in dimnames(dtg)[[2]]) {
      lines(x, dtg[, n], type = "o",
            col = col[n], pch = pch[n], lty = 1, lwd = 3, cex = 1.5, bg = "white");
      if (!is.null(errg)) {
         arrows(x, dtg[, n] - errg[, n], x, dtg[, n] + errg[, n],
                length = .05, angle = 90, code = 4, lwd = 3);
      }
      text(x[lastIndex] + xinch(.2), dtg[lastIndex, n], sprintf("%s targets", n),
           col = col[n], cex = .7, adj = 0);
   }
}

do.fig0103();
rm(do.fig0103);
