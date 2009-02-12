### fig0504.r: scatter-plot of weibull baseline against zero-gap RT for each
### subject
###
### $LastChangedDate$

do.fig0504 <- function () {
   infile.baseline <- "data05.rda";
   infile.control <- "data01.rda";
   outfile <- "fig0504.pdf";
   thisfile <- "fig0504.r";
   exit.function <- function () {
      if (exists("opar")) par(opar);
      if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off();
   }
   on.exit(exit.function());

   if (IsFileUpToDate(outfile, c(infile.baseline, infile.control, thisfile))) {
      warning("Output file is up to date, no action taken");
      return(invisible(NULL));
   }
   load(infile.control);
   data01$gapdur <- as.numeric(as.character(data01$gapdur));
   data01$ntargets <- as.numeric(as.character(data01$ntargets));
   control <- with(data01[data01$gapdur == "0" & data01$target == "1", ],
                   tapply(rt, list(sub, ntargets), mean));

   load(infile.baseline);
   baseline <- with(data05$fit, tapply(baseline, list(sub, gapdur, ntargets), mean));
   Subjects <- dimnames(baseline)[[1]];

   ylim <- c(400, 1000);

   pdf(outfile, width = 6, height = 6, pointsize = 12);
   opar <- par(mfrow = c(1, 1), las = 1, pty = "s", cex.axis = .6,
               xpd = NA, bg = "white");
   for (gd in dimnames(baseline)[[2]]) {
      for (nt in dimnames(baseline)[[3]]) {
         plot(control[Subjects, nt], baseline[Subjects, gd, nt], type = "n",
              xlim = ylim, ylim = ylim, bty = "o",
              xlab = "Gap 0 RT (ms)", ylab = "Estimated baseline from fitted Weibull (ms)",
              main = sprintf("Probetrack2 - Gap %s - %s targets", gd, nt));
         text(control[, nt], baseline[, gd, nt], dimnames(control)[[1]], cex = 1);
      }
   }
}

do.fig0504();
rm(do.fig0504);
