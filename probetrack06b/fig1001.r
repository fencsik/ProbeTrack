### fig1001.r: plot average median RT by probe delay, separated by # targets
###
### $LastChangedDate$

do.fig1001 <- function () {
   infile <- "data10.rda";
   outfile <- "fig1001.pdf";
   thisfile <- "fig1001.r";
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
   data10$soa <- as.numeric(as.character(data10$soa));
   data10$ntargets <- as.numeric(as.character(data10$ntargets));
   data10$gapdur <- as.numeric(as.character(data10$gapdur));
   data10 <- data10[data10$gapdur > 0, ];

   ## extract relevant data
   dt <- with(data10, tapply(rt, list(soa, ntargets), mean, na.rm = TRUE));
   x <- as.numeric(dimnames(dt)[[1]]);

   ## CIs based on MSE from ntargets main effect
   ##errg <- sqrt(5641 / 8) * qt(.975, 21);
   ## CIs based on MSE from average of ntargets effects at each SOA
   ##errg <- sqrt(mean(c(3346, 2208, 1239, 1644)) / 8) * qt(.975, 21);
   ## CIs based on MSE of soa main effect
   errg <- sqrt(1233 / 8) * qt(.975, 21);

   ## settings
   ylim <- c(450, 750);
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
           xlab = "Probe delay (ms)", ylab = "Correct median reaction time (ms)", main = "ProbeTrack6b");
   axis(1, x);
   axis(2);

   for (nt in dimnames(dt)[[2]]) {
      if (!is.null(errg)) {
         arrows(x, dt[, nt] - errg, x, dt[, nt] + errg,
                length = .05, angle = 90, code = 3, lwd = 1, col = col[nt], lty = 1);
      }
      lines(x, dt[, nt], type = "o",
            col = col[nt], pch = pch[nt], lty = lty[nt], lwd = 3, cex = 1.5, bg = "white");
   }
   legend("topright", paste(cond.names, "target(s)"),
          col = col, pch = pch,
          lty = lty, lwd = 3, pt.bg = "white", pt.cex = 1.5,
          bty = "n", ncol = 1, y.intersp = 1.3, cex = .8);
}

do.fig1001();
rm(do.fig1001);
