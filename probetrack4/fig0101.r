### fig0101.r: plot correct RT by probe delay separated by number of targets
###
### $LastChangedDate$

do.an06 <- function () {
   infile <- "data01.rda";
   pdffile <- "fig0101.pdf";
   exit.function <- function () {
      if (exists("opar")) par(opar);
      if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off();
   }
   on.exit(exit.function());

   if (!file.exists(infile)) stop("cannot open input file ", infile);
   load(infile);

   ### settings
   ylim <- c(500, 1000);
   cond.names <- as.character(unique(dtg$nTargets));
   nCond <- length(cond.names);
   col <- rainbow(nCond);                               names(col) <- cond.names;
   pch <- c(21, 22, 23, 24, 16, 15, 17, 18)[1:nCond];   names(pch) <- cond.names;
   lty <- 2:1; names(lty) <- as.character(0:1);

   dt <- data01[data01$gapDuration > 0 & data01$acc == 1, ];
   dtg <- apply(tapply(dt$rt, list(dt$soa, dt$nTargets, dt$target, dt$sub), mean),
                1:3, mean, na.rm = T);
   errg <- NULL;
   x <- as.numeric(dimnames(dtg)[[1]]) * 1000 / 75;

   pdf(pdffile, width = 6, height = 6, pointsize = 12);
   opar <- par(mfrow = c(1, 1), las = 1, pty = "s", cex.axis = .6,
               xpd = NA, bg = "white");

   matplot(x, dtg[,,1], type = "n", bty = "n",
           ylim = ylim, axes = F,
           xlab = "Probe delay (ms)", ylab = "Reaction time (ms)", main = "ProbeTrack4/3B");
   axis(1, x);
   axis(2);

   lastIndex <- dim(dtg)[1];
   for (targ in dimnames(dtg)[[3]]) {
      for (n in dimnames(dtg)[[2]]) {
         lines(x, dtg[, n, targ], type = "o",
               col = col[n], pch = pch[n], lty = lty[targ], lwd = 3, cex = 1.5, bg = "white");
         if (!is.null(errg)) {
            arrows(x, dtg[, n, targ] - errg[, n, targ], x, dtg[, n, targ] + errg[, n, targ],
                   length = .05, angle = 90, code = 4, lwd = 3);
         }
         if (targ == "1") {
            text(x[lastIndex] + xinch(.2), dtg[lastIndex, n, targ], sprintf("%s targets", n),
                 col = col[n], cex = .7, adj = 0);
         }
      }
   }
   legend(min(x), max(ylim), c("probe distractor", "probe target"),
          lty = lty, lwd = 3,
          y.intersp = 1.3, bty = "n");
}

do.an06();
rm(do.an06);
