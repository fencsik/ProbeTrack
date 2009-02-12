### an0603: t-tests comparing RT on gap trials at each probe delay to RT on
### no-gap trials
###
### $LastChangedDate$

do.an0603 <- function () {
   thisfile <- "an0603.r";
   infile <- "data06.rda";
   outfile <- "an0603.txt";

   exit.function <- function () {
      while (sink.number() > 0) sink();
   }
   on.exit(exit.function());

   if (!file.exists(infile)) stop("cannot open input file ", infile);
   if (IsFileUpToDate(outfile, c(thisfile, infile))) {
      warning("Output file is up to date, no action taken");
      return(invisible(NULL));
   }
   load(infile);

   weibull <- data06$weibull;
   fit <- data06$fit[1, ];
   thresh <- 0.05;
   cutoff <- (fit$asymptote - fit$baseline) * thresh + fit$baseline;
   x <- 0:10000;
   w <- weibull(x, fit$slope, fit$threshold,
                   fit$baseline, fit$asymptote);
   sink(outfile);
   if (any(w <= cutoff)) {
      cat(sprintf("Average function falls to %0.1f%% at %0.0f ms\n",
                  100 * thresh, x[w <= cutoff][1]));
   } else {
      cat(sprintf("Function failed to fall below %0.1f%% by %0.0.f ms\n",
                  100 * thresh, max(x)));
   }
}

do.an0603();
rm(do.an0603);
