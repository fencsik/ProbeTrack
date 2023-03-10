### fig0203.r: plot d' as a function of # targets, separated by probe delay
###
### $LastChangedDate$

do.fig0203 <- function () {
   infile <- "data02.rda";
   outfile <- "fig0203.pdf";
   thisfile <- "fig0203.r";
   exit.function <- function () {
      if (exists("opar")) par(opar);
      if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off();
   }
   on.exit(exit.function());

   if (!file.exists(infile)) stop("cannot open input file ", infile);
   if (IsFileUpToDate(outfile, c(infile, thisfile))) {
      warning("Output file is up to date, no action taken");
      return(invisible(NULL));
   }
   load(infile);
   data02$soa <- as.numeric(as.character(data02$soa));
   data02$ntargets <- as.numeric(as.character(data02$ntargets));
   data02$gapdur <- as.numeric(as.character(data02$gapdur));
   data02 <- data02[data02$gapdur > 0, ];

   ## extract relevant data
   dt <- with(data02, tapply(dprime, list(ntargets, soa), mean, na.rm = TRUE));
   x <- as.numeric(dimnames(dt)[[1]]);

   ## CIs based on MSE of soa main effect
   errg <- sqrt(.3308 / 8) * qt(.975, 14);
   ## CIs based on MSE from ntargets main effect
   ##errg <- sqrt(.4555 / 8) * qt(.975, 14);
   ## CIs based on MSE from average of ntargets effects at each SOA
   ##errg <- sqrt(mean(c(.5587, .5028, .3407, .4254)) / 8) * qt(.975, 14);

   ## settings
   ylim <- c(0, 4);
   cond.names <- dimnames(dt)[[2]];
   nCond <- length(cond.names);
   col <- rainbow(nCond);                               names(col) <- cond.names;
   lty <- rep(1, nCond);                                names(lty) <- cond.names;
   pch <- c(21, 22, 23, 24, 16, 15, 17, 18)[1:nCond];   names(pch) <- cond.names;

   pdf(outfile, width = 6, height = 6, pointsize = 12);
   opar <- par(mfrow = c(1, 1), las = 1, pty = "m", cex.axis = .6,
               xpd = NA, bg = "white");

   matplot(x, dt, type = "n", bty = "n",
           ylim = ylim, axes = F,
           xlab = "Number of targets", ylab = "d'", main = "ProbeTrack4");
   axis(1, x);
   axis(2);

   for (soa in dimnames(dt)[[2]]) {
      if (!is.null(errg)) {
         arrows(x, dt[, soa] - errg, x, dt[, soa] + errg,
                length = .05, angle = 90, code = 3, lwd = 1, col = col[soa], lty = 1);
      }
      lines(x, dt[, soa], type = "o",
            col = col[soa], pch = pch[soa], lty = lty[soa], lwd = 3, cex = 1.5, bg = "white");
   }
   legend("topright", max(ylim), paste(cond.names, "ms"),
          col = col, pch = pch,
          lty = lty, lwd = 3, pt.bg = "white", pt.cex = 1.5,
          bty = "n", ncol = 1, y.intersp = 1.3, cex = .8);
}

do.fig0203();
rm(do.fig0203);
