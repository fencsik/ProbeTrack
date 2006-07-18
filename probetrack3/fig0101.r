### fig0101.r: plot correct RT by probe delay separated by probe type and gap
### duration
###
### $LastChangedDate$

do.fig0101 <- function () {
   infile <- "data01.rda";
   outfile <- "fig0101.pdf";
   thisfile <- "fig0101.r";
   exit.function <- function () {
      if (exists("opar")) par(opar);
      if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off();
   }
   on.exit(exit.function());

   if (!file.exists(infile)) stop("cannot open input file ", infile);
   if (file.exists(outfile) &&
       file.info(outfile)$mtime > file.info(thisfile)$mtime &&
       file.info(outfile)$mtime > file.info(infile)$mtime) {
      warning("Output file is up to date, no action taken");
      return(NULL);
   }
   load(infile);
   data01$soa <- as.numeric(as.character(data01$soa));
   data01$gapdur <- round(as.numeric(as.character(data01$gapdur)) * 1000 / 75, 0);

   ## extract relevant data
   dtg <- with(data01,
               tapply(rt.cor, list(soa, target, gapdur), mean, na.rm = TRUE));
   errg <- with(data01,
                tapply(rt.cor, list(soa, target, gapdur),
                       function(x) sqrt(var(x, na.rm = TRUE) / length(x))));

   ## fix zero-gap condition
   for (targ in dimnames(dtg)[[2]]) {
      dtg[, targ, "0"] <- mean(dtg[, targ, "0"], na.rm = T);
   }

   x <- as.numeric(dimnames(dtg)[[1]]) * 1000 / 75;

   ## settings
   ylim <- c(500, 1000);
   cond.names <- dimnames(dtg)[[3]];
   nCond <- length(cond.names);
   col <- rainbow(nCond);                               names(col) <- cond.names;
   pch <- c(21, 22, 23, 24, 16, 15, 17, 18)[1:nCond];   names(pch) <- cond.names;
   pch["0"] <- NA;

   pdf(outfile, width = 6, height = 6, pointsize = 12);
   opar <- par(mfrow = c(1, 1), las = 1, pty = "m", cex.axis = .6,
               xpd = NA, bg = "white");

   lastIndex <- dim(dtg)[1];
   for (targ in dimnames(dtg)[[2]]) {

      matplot(x, dtg[, targ, ], type = "n", bty = "n",
              ylim = ylim, axes = F,
              xlab = "Probe delay (ms)", ylab = "Probe RT (ms)", main = "ProbeTrack3");
      axis(1, x);
      axis(2);

      for (gd in dimnames(dtg)[[3]]) {
         if (!is.null(errg) && gd != "0") {
            arrows(x, dtg[, targ, gd] - errg[, targ, gd], x, dtg[, targ, gd] + errg[, targ, gd],
                   length = .05, angle = 90, code = 3, lwd = 1, col = col[gd], lty = 1);
         }
         lines(x, dtg[, targ, gd], type = "o",
               col = col[gd], pch = pch[gd], lty = 1, lwd = 3, cex = 1.5, bg = "white");
      }
      legend(max(x) - xinch(2), max(ylim), paste(cond.names, "ms Gap"),
             col = col, pch = pch,
             lty = 1, lwd = 3, pt.bg = "white", pt.cex = 1.5,
             bty = "n", y.intersp = 1.3, cex = .8);
      legend(max(x) - xinch(3.5), max(ylim), c("gap", "no gap"),
             lty = 1:2, lwd = 3,
             bty = "n", y.intersp = 1.3, cex = .8);
   }
}

do.fig0101();
rm(do.fig0101);
