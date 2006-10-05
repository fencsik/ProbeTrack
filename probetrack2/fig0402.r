### fig0402.r: plot fit of weibull against observed data, averaged across
### subjects
###
### $LastChangedDate$

do.fig0402 <- function () {
   infile <- "data04.rda";
   outfile <- "fig0402.pdf";
   errfile <- "an0402.rda";
   thisfile <- "fig0402.r";
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
   weibull <- data04$weibull;

   dt <- with(data04$data, tapply(rt.cor, list(soa, gapdur, ntargets), mean));
   pred <- with(data04$data, tapply(rt.pred, list(soa, gapdur, ntargets), mean));
   baseline <- with(data04$fit, tapply(baseline, list(gapdur, ntargets), mean));
   fit <- with(data04$fit, aggregate(data.frame(slope = slope, threshold = threshold,
                                                baseline = baseline, asymptote = asymptote),
                                     list(gapdur = gapdur, ntargets = ntargets), mean));
   predx <- seq(0, 100, by = 1);
   ##predy <- array(dim = c(length(predx

   if (!file.exists(errfile)) stop("cannot open error file ", errfile);
   load(errfile);
   ci <- an0402[, , , "ci"];

   x <- as.numeric(dimnames(dt)[[1]]);
   gapdurList <- dimnames(dt)[[2]];
   ntargetsList <- dimnames(dt)[[3]];

   pdf(outfile, width = 6, height = 6, pointsize = 12);
   opar <- par(mfrow = c(1, 1), las = 1, pty = "m", cex.axis = .6,
               xpd = NA, bg = "white");

   ## set up color matrix
   ylim <- c(500, 1000);
   col <- as.character(with(data04$data, tapply(rt.cor, list(gapdur, ntargets), mean)));
   col <- matrix(rainbow(length(gapdurList) * length(ntargetsList)),
                 nrow = length(gapdurList), ncol = length(ntargetsList),
                 dimnames = list(gapdurList, ntargetsList));

   plot(x, dt[, 1, 1], type = "n", bty = "n",
        axes = F, ylim = ylim,
        xlab = "Probe delay (ms)", ylab = "Probe RT (ms)", main = "ProbeTrack2");
   axis(1, x, x * 1000 / 75);
   axis(2);
   for (gd in gapdurList) {
      for (nt in ntargetsList) {
         index <- fit$gapdur == gd & fit$ntargets == nt;
         abline(h = baseline[gd, nt], xpd = F,
               col = col[gd, nt], lwd = 2, lty = 3);
         lines(x, pred[, gd, nt], type = "o",
               col = col[gd, nt], bg = "white", pch = 4, cex = 1.5, lwd = 3, lty = 2);
         arrows(x, dt[, gd, nt] - ci, x, dt[, gd, nt] + ci,
                length = .05, angle = 90, code = 3, lwd = 1, col = col[gd, nt], lty = 1);
         lines(x, dt[, gd, nt], type = "o",
               col = col[gd, nt], bg = "white", pch = 21, cex = 1.5, lwd = 3, lty = 1);
      }
   }
}

do.fig0402();
rm(do.fig0402);
