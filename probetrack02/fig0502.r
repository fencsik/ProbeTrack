### fig0502.r: plot fit of weibull against observed data, averaged across
### subjects
###
### $LastChangedDate$

do.fig0502 <- function () {
   infile <- "data05.rda";
   outfile <- "fig0502.pdf";
   errfile <- "an0502.rda";
   thisfile <- "fig0502.r";
   exit.function <- function () {
      if (exists("opar")) par(opar);
      if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off();
   }
   on.exit(exit.function());

   if (!file.exists(infile)) stop("cannot open input file ", infile);
   if (IsFileUpToDate(outfile, c(infile, thisfile, errfile))) {
      warning("Output file is up to date, no action taken");
      return(invisible(NULL));
   }
   load(infile);
   weibull <- data05$weibull;

   dt <- with(data05$data, tapply(rt, list(soa, gapdur, ntargets), mean));

   ## gather parameters for weibull fits
   attach(data05$fit);
   factors <- list(sub, gapdur, ntargets);
   slope <- tapply(slope, factors, mean);
   threshold <- tapply(threshold, factors, mean);
   baseline <- tapply(baseline, factors, mean);
   asymptote <- tapply(asymptote, factors, mean);
   detach();

   ## fit weibull to each subject in each condition
   Subjects <- dimnames(slope)[[1]];
   predx <- seq(0, 1300, by = 1);
   predy <- array(dim = c(length(Subjects), length(predx), dim(slope)[2], dim(slope)[3]),
                  dimnames = list(Subjects, 1:length(predx), dimnames(slope)[[2]], dimnames(slope)[[3]]));
   for (sub in dimnames(predy)[[1]]) {
      for (gd in dimnames(predy)[[3]]) {
         for (nt in dimnames(predy)[[4]]) {
            predy[sub, , gd, nt] <- weibull(predx, slope[sub, gd, nt], threshold[sub, gd, nt],
                                            baseline[sub, gd, nt], asymptote[sub, gd, nt]);
         }
      }
   }
   predy <- apply(predy, 2:4, mean);
   baseline <- apply(baseline, 2:3, mean);

   if (!file.exists(errfile)) stop("cannot open error file ", errfile);
   load(errfile);
   ci <- an0502[, , , "ci"];

   x <- as.numeric(dimnames(dt)[[1]]);
   gapdurList <- dimnames(dt)[[2]];
   ntargetsList <- dimnames(dt)[[3]];

   pdf(outfile, width = 6, height = 6, pointsize = 12);
   opar <- par(mfrow = c(1, 1), las = 1, pty = "m", cex.axis = .6,
               xpd = NA, bg = "white");

   ylim <- c(500, 1000);

   plot(x, dt[, 1, 1], type = "n", bty = "n",
        axes = F, ylim = ylim,
        xlab = "Probe delay (ms)", ylab = "Probe RT (ms)", main = "ProbeTrack2");
   axis(1, x);
   axis(2);
   for (gd in gapdurList) {
      for (nt in ntargetsList) {
         abline(h = baseline[gd, nt], xpd = F,
               col = 1, lwd = 2, lty = 3);
         lines(predx, predy[, gd, nt], type = "l",
               col = 1, lwd = 2, lty = 1);
         arrows(x, dt[, gd, nt] - ci, x, dt[, gd, nt] + ci,
                length = .05, angle = 90, code = 3, lwd = 1, col = 1, lty = 1);
         points(x, dt[, gd, nt], type = "p",
               col = 1, bg = "white", pch = 21, cex = 1.5, lwd = 3);
      }
   }
}

do.fig0502();
rm(do.fig0502);
