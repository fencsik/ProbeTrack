### fig0701.r: plot fit of weibull against observed data, averaged across
### subjects
###
### $LastChangedDate$

do.fig0701 <- function () {
   infile <- "data07.rda";
   outfile <- "fig0701.pdf";
   thisfile <- "fig0701.r";
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
   weibull <- data07$weibull;

   dt <- with(data07$data, tapply(rt, list(soa, gapdur, ntargets, target), mean));

   ## gather parameters for weibull fits
   attach(data07$fit);
   factors <- list(gapdur, ntargets, target);
   slope <- tapply(slope, factors, mean);
   threshold <- tapply(threshold, factors, mean);
   baseline <- tapply(baseline, factors, mean);
   asymptote <- tapply(asymptote, factors, mean);
   detach();

   ## define plotting variables
   col <- array(rainbow(prod(dim(slope))), dim = dim(slope), dimnames = dimnames(slope));

   ## fit weibull to each subject in each condition
   predx <- seq(0, 1300, by = 1);
   predx <- predx[predx < 640];
   predy <- array(dim = c(length(predx), dim(slope)[1], dim(slope)[2], dim(slope)[3]),
                  dimnames = list(seq_along(predx), dimnames(slope)[[1]],
                    dimnames(slope)[[2]], dimnames(slope)[[3]]));
   for (gd in dimnames(predy)[[2]]) {
      for (nt in dimnames(predy)[[3]]) {
         for (targ in dimnames(predy)[[4]]) {
            predy[, gd, nt, targ] <- weibull(predx, slope[gd, nt, targ], threshold[gd, nt, targ],
                                             baseline[gd, nt, targ], asymptote[gd, nt, targ]);
         }
      }
   }

   x <- as.numeric(dimnames(dt)[[1]]);

   pdf(outfile, width = 6, height = 6, pointsize = 12);
   opar <- par(mfrow = c(1, 1), las = 1, pty = "m", cex.axis = .6,
               xpd = NA, bg = "white");

   ylim <- c(500, 1000);
   plotx <- x; plotx[length(plotx)] <- max(x) / 2;

   plot(plotx, dt[, 1, 1, 1], type = "n", bty = "n",
        axes = F, ylim = ylim,
        xlab = "Probe delay (ms)", ylab = "Probe RT (ms)", main = "ProbeTrack2");
   axis(1, plotx, x);
   axis(2);
   for (gd in dimnames(dt)[[2]]) {
      for (nt in dimnames(dt)[[3]]) {
         for (targ in dimnames(dt)[[4]]) {
            abline(h = baseline[gd, nt, targ], xpd = F,
                   col = col[gd, nt, targ], lwd = 2, lty = 3);
            lines(predx, predy[, gd, nt, targ], type = "l",
                  col = col[gd, nt, targ], lwd = 2, lty = 1);
            points(plotx, dt[, gd, nt, targ], type = "p",
                   col = col[gd, nt, targ], bg = "white", pch = 21, cex = 1.5, lwd = 3);
         }
      }
   }
}

do.fig0701();
rm(do.fig0701);
