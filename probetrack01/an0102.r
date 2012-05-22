### an0102: anova on accuracy as a function of probe delay and probe type

do.an0102 <- function () {
   infile <- "data01.rda";
   outfile <- "an0102.txt";

   exit.function <- function () {
      while (sink.number() > 0) sink();
   }
   on.exit(exit.function());

   load(infile);

   sink(outfile);
   cat("ANOVA on accuracy as a function of probe type and probe delay\n");
   cat("  gap trials only\n");
   print(summary(aov(pcor ~ target * soa + Error(sub / (target * soa)),
                     data01[data01$gapdur == "10",])));
}

do.an0102();
rm(do.an0102);
