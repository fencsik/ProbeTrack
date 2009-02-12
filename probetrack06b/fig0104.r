### fig0104.r: plot accuracy by probe delay separated by number of targets
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

   if (!file.exists(infile)) stop("cannot open input file ", infile);
   if (IsFileUpToDate(outfile, c(infile, thisfile))) {
      warning("Output file is up to date, no action taken");
      return(invisible(NULL));
   }
   load(infile);
   data01$soa <- as.numeric(as.character(data01$soa));
   data01$ntargets <- as.numeric(as.character(data01$ntargets));
   data01$gapdur <- as.numeric(as.character(data01$gapdur));
   data01 <- data01[data01$gapdur > 0, ];

   ## extract relevant data
   dt <- with(data01, tapply(pcor, list(soa, ntargets), mean, na.rm = TRUE));
   x <- as.numeric(dimnames(dt)[[1]]);

   ## CIs based on MSE from ntargets main effect
   errg <- sqrt(19414 / 8) * qt(.975, 21);
   ## CIs based on MSE from average of ntargets effects at each SOA
   ##errg <- sqrt(mean(c(10014, 6608, 5813, 10025)) / 8) * qt(.975, 21);
   ## CIs based on MSE of soa main effect
   ##errg <- sqrt(5015 / 8) * qt(.975, 21);
   errg <- NULL;

   ## settings
   ylim <- c(0.5, 1.0);
   cond.names <- dimnames(dt)[[2]];
   nCond <- length(cond.names);
   col <- rainbow(nCond);                               names(col) <- cond.names;
   lty <- rep(1, nCond);                                names(lty) <- cond.names;
   pch <- c(21, 22, 23, 24, 16, 15, 17, 18)[1:nCond];   names(pch) <- cond.names;

   pdf(outfile, width = 6, height = 6, pointsize = 12);
   opar <- par(mfrow = c(1, 1), las = 1, pty = "s", cex.axis = .6,
               xpd = NA, bg = "white");

   matplot(x, dt, type = "n", bty = "n",
           ylim = ylim, axes = F,
           xlab = "Probe delay (ms)", ylab = "Proportion correct",
           main = "ProbeTrack6b");
   axis(1, x);
   axis(2);

   lastIndex <- dim(dt)[1];
   for (n in dimnames(dt)[[2]]) {
      if (!is.null(errg)) {
         arrows(x, dt[, n] - errg[, n], x, dt[, n] + errg[, n],
                length = .05, angle = 90, code = 3, lwd = 1, col = col[n], lty = 1);
      }
      lines(x, dt[, n], type = "o",
            col = col[n], pch = pch[n], lty = 1, lwd = 3, cex = 1.5, bg = "white");
   }
   legend("bottom", max(ylim), paste(cond.names, "target(s)     "),
          col = col, pch = pch,
          lty = lty, lwd = 3, pt.bg = "white", pt.cex = 1.5,
          bty = "n", ncol = 2, y.intersp = 1.3, cex = .8);
}

do.fig0104();
rm(do.fig0104);
