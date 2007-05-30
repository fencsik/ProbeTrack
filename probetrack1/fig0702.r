### fig0702.r: plot fit of model against observed data, averaged across
### subjects
###
### $LastChangedDate$

do.fig0702 <- function () {
   infile <- "data07.rda";
   outfile <- "fig0702.pdf";
   errfile <- "an0702.rda";
   thisfile <- "fig0702.r";
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
   model <- data07$model;

   dt <- with(data07$data, tapply(rt, list(soa, gapdur, ntargets), mean));

   ## gather parameters for model fits
   attach(data07$fit);
   factors <- list(sub, gapdur, ntargets);
   rtime <- tapply(rtime, factors, mean);
   baseRT <- tapply(baseRT, factors, mean);
   detach();

   ## fit model to each subject in each condition
   Subjects <- dimnames(rtime)[[1]];
   predx <- seq(0, 1280, by = 1);
   predy <- array(dim = c(length(Subjects), length(predx), dim(rtime)[2], dim(rtime)[3]),
                  dimnames = list(Subjects, 1:length(predx), dimnames(rtime)[[2]], dimnames(rtime)[[3]]));
   for (sub in dimnames(predy)[[1]]) {
      for (gd in dimnames(predy)[[3]]) {
         for (nt in dimnames(predy)[[4]]) {
            predy[sub, , gd, nt] <- model(predx, rtime[sub, gd, nt], baseRT[sub, gd, nt]);
         }
      }
   }
   predy <- apply(predy, 2:4, mean);
   baseRT <- apply(baseRT, 2:3, mean);

###    if (!file.exists(errfile)) stop("cannot open error file ", errfile);
###    load(errfile);
###    ci <- an0702[, , , "ci"];

   x <- as.numeric(dimnames(dt)[[1]]);
   gapdurList <- dimnames(dt)[[2]];
   ntargetsList <- dimnames(dt)[[3]];

   pdf(outfile, width = 6, height = 6, pointsize = 12);
   opar <- par(mfrow = c(1, 1), las = 1, pty = "m", cex.axis = .6,
               xpd = NA, bg = "white");

   ylim <- c(500, 1000);

   plot(x, dt[, 1, 1], type = "n", bty = "n",
        axes = F, ylim = ylim,
        xlab = "Probe delay (ms)", ylab = "Probe RT (ms)", main = "ProbeTrack1");
   axis(1, x);
   axis(2);
   for (gd in gapdurList) {
      for (nt in ntargetsList) {
         abline(h = baseRT[gd, nt], xpd = F,
               col = 1, lwd = 2, lty = 3);
         lines(predx, predy[, gd, nt], type = "l",
               col = 1, lwd = 2, lty = 1);
###          arrows(x, dt[, gd, nt] - ci, x, dt[, gd, nt] + ci,
###                 length = .05, angle = 90, code = 3, lwd = 1, col = 1, lty = 1);
         points(x, dt[, gd, nt], type = "p",
               col = 1, bg = "white", pch = 21, cex = 1.5, lwd = 3);
      }
   }
}

do.fig0702();
rm(do.fig0702);