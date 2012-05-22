### an0403: t-tests comparing RT on gap trials at each probe delay to RT on
### no-gap trials

do.an0403 <- function () {
   infile <- "data04.rda";
   outfile <- "an0403.txt";

   exit.function <- function () {
      while (sink.number() > 0) sink();
   }
   on.exit(exit.function());

   load(infile);

   weibull <- data04$weibull;
   fit <- data04$fit;
   Subjects <- sort(unique(as.character(fit$sub)));
   cutoff <- (fit$asymptote - fit$baseline) * 0.05 + fit$baseline;
   tBaseline <- rep(-1, length(Subjects));
   names(tBaseline) <- Subjects;
   nSamples <- 500;
   x <- 0:10000;
   Subjects <- names(tBaseline[tBaseline < 0]);
   for (sub in Subjects) {
      index <- which(fit$sub == sub);
      w <- weibull(x, fit[index, "slope"], fit[index, "threshold"],
                   fit[index, "baseline"], fit[index, "asymptote"]);
      if (any(w <= cutoff[index])) {
         tBaseline[sub] <- x[which(w <= cutoff[index])[1]];
      }
   }
   if (any(tBaseline == -1)) {
      warning(sprintf("No limit found by %0.0f ms for %0.0f subjects",
                      max(x), sum(tBaseline == -1)));
   }

   sink(outfile);
   cat("Estimate of time at which fitted weibull hits baseline:\n\n");
   print(tBaseline);
}

do.an0403();
rm(do.an0403);
