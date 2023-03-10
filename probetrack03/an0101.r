### an0101: anova on RT as a function of probe delay, gap duration, and probe
### type
###
### $LastChangedDate$

do.an0101 <- function () {
   thisfile <- "an0101.r";
   infile <- "data01.rda";
   outfile <- "an0101.txt";

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
   cat("ANOVA on correct RT as a function of probe type, gap duration, and probe delay\n");
   cat("  gap trials only\n");
   print(summary(aov(rt ~ target * gapdur * soa + Error(sub / (target * gapdur * soa)),
                     data01[data01$gapdur != "0",])));

   cat("\n\n\n");
   cat("ANOVA on all RT as a function of probe type, gap duration, and probe delay\n");
   cat("  gap trials only\n");
   print(summary(aov(rt.all ~ target * gapdur * soa + Error(sub / (target * gapdur * soa)),
                     data01[data01$gapdur != "0",])));
}

do.an0101();
rm(do.an0101);
