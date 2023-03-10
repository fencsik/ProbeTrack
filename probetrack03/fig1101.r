### fig1101.r: plot fit of model against observed data for each subject
###
### $LastChangedDate$

do.fig1101 <- function () {
   infile <- "data11.rda";
   outfile <- "fig1101.pdf";
   thisfile <- "fig1101.r";
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
   model <- data11$model;

   dt <- with(data11$data, tapply(rt, list(sub, soa, gapdur, ntargets), mean));

   ## gather parameters for model fits
   attach(data11$fit);
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

   x <- as.numeric(dimnames(dt)[[2]]);
   gapdurList <- dimnames(dt)[[3]];
   ntargetsList <- dimnames(dt)[[4]];

   ## plot settings
   condNames <- gapdurList;
   nCond <- length(condNames);
   col <- rainbow(nCond); names(col) <- condNames;
   pch <- c(21, 23, 24);  names(pch) <- condNames;

   pdf(outfile, width = 8, height = 8, pointsize = 12);
   opar <- par(mfrow = c(2, 2), las = 1, pty = "m", cex.axis = .6,
               xpd = NA, bg = "white");

   ylim <- c(300, 1000);

   counter <- 0;
   for (sub in Subjects) {
      matplot(x, dt[sub, , , 1], type = "n", bty = "n",
              axes = F, ylim = ylim,
              xlab = "Probe delay (ms)", ylab = "Probe RT (ms)",
              main = sprintf("ProbeTrack1 - %s", sub));
      for (gd in gapdurList) {
         axis(1, x);
         axis(2);
         for (nt in ntargetsList) {
            abline(h = baseRT[sub, gd, nt], xpd = F,
                   col = col[gd], lwd = 2, lty = 3);
            lines(predx, predy[sub, , gd, nt], type = "l",
                  col = col[gd], lwd = 2, lty = 1);
            points(x, dt[sub, , gd, nt], type = "p",
                   col = col[gd], bg = "transparent", pch = 21, cex = 1.5, lwd = 1);
         }
      }
      if (counter %% 4 == 0) {
         legend("bottomright", sprintf("%s ms", gapdurList), inset = c(-.4, -.45),
                bty = "n", col = col, lwd = 2, ncol = 1);
      }
      counter <- counter + 1;
   }
}

do.fig1101();
rm(do.fig1101);
