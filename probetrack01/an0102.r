### an0102: anova on accuracy as a function of probe delay and probe type
###
### $LastChangedDate$

do.an0102 <- function () {
   thisfile <- "an0102.r";
   infile <- "data01.rda";
   outfile <- "an0102.txt";

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
   cat("ANOVA on accuracy as a function of probe type and probe delay\n");
   cat("  gap trials only\n");
   print(summary(aov(pcor ~ target * soa + Error(sub / (target * soa)),
                     data01[data01$gapdur == "10",])));
}

do.an0102();
rm(do.an0102);
