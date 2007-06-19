### fig0102.r: plot accuracy by probe delay separated by number of targets and
### probe type
###
### $LastChangedDate$

do.fig0102 <- function () {
   infile <- "data01.rda";
   outfile <- "fig0102.pdf";
   thisfile <- "fig0102.r";
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
   dt <- with(data01, tapply(pcor, list(soa, ntargets, target), mean, na.rm = TRUE));
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

   pdf(outfile, width = 9.5, height = 7, pointsize = 12);
   opar <- par(mfrow = c(1, 2), las = 1, pty = "s", cex.axis = .6,
               xpd = NA, bg = "white");

   counter <- 0;
   for (targ in dimnames(dt)[[3]]) {
      probe <- ifelse(targ == "1", "Target", "Distractor");
      matplot(x, dt[,,targ], type = "n", bty = "n",
              ylim = ylim, axes = F,
              main = "ProbeTrack6",
              xlab = sprintf("%s probe delay (ms)", probe), 
              ylab = "");
      axis(1, x);
      axis(2);
      if (counter %% 2 == 0) title(ylab = "Proportion correct");

      lastIndex <- dim(dt)[1];
      for (n in dimnames(dt)[[2]]) {
         if (!is.null(errg)) {
            arrows(x, dt[, n, targ] - errg[, n, targ], x, dt[, n, targ] + errg[, n, targ],
                   length = .05, angle = 90, code = 3, lwd = 1, col = col[n], lty = 1);
         }
         lines(x, dt[, n, targ], type = "o",
               col = col[n], pch = pch[n], lty = lty[n], lwd = 3, cex = 1.5, bg = "white");
      }
      if (counter %% 2 == 0) {
         legend("topleft", paste(cond.names, "target(s)     "), inset = c(.9, -.05),
                col = col, pch = pch,
                lty = lty, lwd = 3, pt.bg = "white", pt.cex = 1.5,
                bty = "n", y.intersp = 1.3, ncol = 1, cex = .8);
      }
      counter <- counter + 1;
   }
}

do.fig0102();
rm(do.fig0102);
