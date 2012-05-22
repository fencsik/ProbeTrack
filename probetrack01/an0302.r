### an0302: anova on accuracy as a function of probe delay and probe type

do.an0302 <- function () {
   infile <- "data03.rda";
   outfile <- "an0302.txt";

   exit.function <- function () {
      while (sink.number() > 0) sink();
   }
   on.exit(exit.function());

   load(infile);

   sink(outfile);
   cat("ANOVA on accuracy as a function of probe delay\n");
   cat("  gap trials only\n");
   print(summary(aov(pcor ~ soa + Error(sub / soa),
                     data03[data03$gapdur == "10",])));
}

do.an0302();
rm(do.an0302);
