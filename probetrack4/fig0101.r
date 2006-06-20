### fig0101.r: plot correct RT by probe delay separated by number of targets and
### probe type
###
### $LastChangedDate$

do.fig0101 <- function () {
   infile <- "data01.rda";
   pdffile <- "fig0101.pdf";
   exit.function <- function () {
      if (exists("opar")) par(opar);
      if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off();
   }
   on.exit(exit.function());

   if (!file.exists(infile)) stop("cannot open input file ", infile);
   load(infile);
   data01$ntargets <- as.numeric(as.character(data01$ntargets));
   data01$soa <- as.numeric(as.character(data01$soa));

   ## extract relevant data
   dtg <- with(data01[data01$gapdur != "0", ],
               tapply(rt.cor, list(soa, ntargets, target), mean, na.rm = TRUE));
   errg <- with(data01[data01$gapdur != "0", ],
                tapply(rt.cor, list(soa, ntargets, target),
                       function(x) sqrt(var(x, na.rm = TRUE) / length(x))));
   dtng <- with(data01[data01$gapdur == "0", ],
                tapply(rt.cor, list(soa, ntargets, target), mean, na.rm = TRUE));

   x <- as.numeric(dimnames(dtg)[[1]]) * 1000 / 75;

   ## settings
   ylim <- c(500, 1000);
   cond.names <- dimnames(dtg)[[2]];
   nCond <- length(cond.names);
   col <- rainbow(nCond);                               names(col) <- cond.names;
   pch <- c(21, 22, 23, 24, 16, 15, 17, 18)[1:nCond];   names(pch) <- cond.names;
   lty <- rep(1, 2); names(lty) <- dimnames(dtg)[[3]];

   pdf(pdffile, width = 6, height = 6, pointsize = 12);
   opar <- par(mfrow = c(1, 1), las = 1, pty = "s", cex.axis = .6,
               xpd = NA, bg = "white");


   for (targ in dimnames(dtg)[[3]]) {
      probe <- ifelse(targ == "1", "Target", "Distractor");
      matplot(x, dtg[,,targ], type = "n", bty = "n",
              ylim = ylim, axes = F,
              xlab = sprintf("%s probe delay (ms)", probe), 
              ylab = sprintf("%s-probe RT (ms)", probe), main = "ProbeTrack4/3B");
      axis(1, x);
      axis(2);

      lastIndex <- dim(dtg)[1];
      for (n in dimnames(dtg)[[2]]) {
         if (!is.null(errg)) {
            arrows(x, dtg[, n, targ] - errg[, n, targ], x, dtg[, n, targ] + errg[, n, targ],
                   length = .05, angle = 90, code = 3, lwd = 1, col = col[n], lty = 1);
         }
         lines(x, dtg[, n, targ], type = "o",
               col = col[n], pch = pch[n], lty = lty[targ], lwd = 3, cex = 1.5, bg = "white");
         text(x[lastIndex] + xinch(.2), dtg[lastIndex, n, targ], sprintf("%s targets", n),
              col = col[n], cex = .7, adj = 0);
      }
   }
}

do.fig0101();
rm(do.fig0101);
