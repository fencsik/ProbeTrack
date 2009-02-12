### fig1202.r: plot fit of model against observed data, averaged across
### subjects
###
### $LastChangedDate$

do.fig1202 <- function () {
   infile <- "data12.rda";
   outfile <- "fig1202.pdf";
   thisfile <- "fig1202.r";
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
   model <- data12$model;

   dt <- with(data12$data, tapply(rt, list(soa), mean));

   ## gather parameters for model fits
   subjects <- sort(unique(rownames(data12$fit)));
   rtime <- with(data12$fit, tapply(rtime, list(subjects), mean));
   baseRT <- with(data12$fit, tapply(baseRT, list(subjects), mean));

   ## fit model to each subject in each condition
   Subjects <- dimnames(rtime)[[1]];
   predx <- seq(0, 1280, by = 1);
   predy <- array(dim = c(length(Subjects), length(predx)),
                  dimnames = list(Subjects, 1:length(predx)));
   for (sub in dimnames(predy)[[1]]) {
      predy[sub, ] <- model(predx, rtime[sub], baseRT[sub]);
   }
   predy <- apply(predy, 2, mean);
   baseRT <- mean(baseRT);

   x <- as.numeric(dimnames(dt)[[1]]);

   pdf(outfile, width = 6, height = 6, pointsize = 12);
   opar <- par(mfrow = c(1, 1), las = 1, pty = "m", cex.axis = .6,
               xpd = NA, bg = "white");

   ylim <- c(500, 1000);

   plot(x, dt, type = "n", bty = "n",
        axes = F, ylim = ylim,
        xlab = "Probe delay (ms)", ylab = "Probe RT (ms)", main = "ProbeTrack4");
   axis(1, x);
   axis(2);
   abline(h = baseRT, xpd = F,
          col = 1, lwd = 2, lty = 3);
   lines(predx, predy, type = "l",
         col = 1, lwd = 2, lty = 1);
   points(x, dt, type = "p",
          col = 1, bg = "white", pch = 21, cex = 1.5, lwd = 3);
}

do.fig1202();
rm(do.fig1202);
