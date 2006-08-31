### an0302: anova on accuracy as a function of probe delay, gap duration, and
### probe type
###
### $LastChangedDate$

do.an0302 <- function () {
   thisfile <- "an0302.r";
   infile <- "data03.rda";
   outfile <- "an0302.txt";

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

   sink(outfile);
   cat("ANOVA on accuracy as a function of gap duration and probe delay\n");
   cat("  gap trials only\n");
   print(summary(aov(pcor ~ gapdur * soa + Error(sub / (gapdur * soa)),
                     data03[data03$gapdur != "0",])));

}

do.an0302();
rm(do.an0302);
