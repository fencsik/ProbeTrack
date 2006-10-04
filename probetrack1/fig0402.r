### fig0402.r: plot fit of weibull against observed data, averaged across
### subjects
###
### $LastChangedDate$

do.fig0402 <- function () {
   infile <- "data04.rda";
   outfile <- "fig0402.pdf";
   thisfile <- "fig0402.r";
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
   weibull <- data04$weibull;

   dt <- with(data04$data, tapply(rt.cor, list(soa, gapdur, ntargets), mean));
   fit <- with(data04$fit, aggregate(data.frame(slope = slope, threshold = threshold,
                                                baseline = baseline, asymptote = asymptote),
                                     list(gapdur = gapdur, ntargets = ntargets), mean));

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
        xlab = "Probe delay (ms)", ylab = "Probe RT (ms)", main = "ProbeTrack1");
   axis(1, x, x * 1000 / 75);
   axis(2);
   for (gd in gapdurList) {
      for (nt in ntargetsList) {
         index <- fit$gapdur == gd & fit$ntargets == nt;
         i <- seq(0, 1000, by = .1);
         y <- weibull(i, fit[index, "slope"], fit[index, "threshold"],
                      fit[index, "baseline"], fit[index, "asymptote"])
         lines(i / 10, y,
               lty = 1, lwd = 2, col = col[gd, nt]);
         points(x, dt[, gd, nt],
                col = col[gd, nt], bg = "white", pch = 21, cex = 2.0);
      }
   }
}

do.fig0402();
rm(do.fig0402);
