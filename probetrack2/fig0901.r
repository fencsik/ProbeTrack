### fig0901.r: plot fit of model against observed data
###
### $LastChangedDate$

do.fig0901 <- function () {
   infile <- "data09.rda";
   outfile <- "fig0901.pdf";
   thisfile <- "fig0901.r";
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
   model <- data09$model;

   dt <- with(data09$data, tapply(rt, list(soa, gapdur, ntargets), mean));

   ## gather parameters for model fits
   attach(data09$fit);
   factors <- list(sub, gapdur, ntargets);
   rtime <- tapply(rtime, factors, mean);
   baseRT <- tapply(baseRT, factors, mean);
   detach();

   ## fit model to each subject in each condition
   Subjects <- dimnames(rtime)[[1]];
   predx <- seq(0, 1300, by = 1);
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
         abline(h = baseRT[gd, nt], xpd = F,
               col = 1, lwd = 2, lty = 3);
         lines(predx, predy[, gd, nt], type = "l",
               col = 1, lwd = 2, lty = 1);
         points(x, dt[, gd, nt], type = "p",
               col = 1, bg = "white", pch = 21, cex = 1.5, lwd = 3);
      }
   }
}

do.fig0901();
rm(do.fig0901);
