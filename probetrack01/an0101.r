### an0101: anova on RT as a function of probe delay and probe type

do.an0101 <- function () {
   infile <- "data01.rda";
   outfile <- "an0101.txt";

   exit.function <- function () {
      while (sink.number() > 0) sink();
   }
   on.exit(exit.function());

   load(infile);

   sink(outfile);
   cat("ANOVA on correct RT as a function of probe type and probe delay\n");
   cat("  gap trials only\n");
   print(summary(aov(rt ~ target * soa + Error(sub / (target * soa)),
                     data01[data01$gapdur == "10",])));

   cat("\n\n\n");
   cat("ANOVA on all RT as a function of probe type and probe delay\n");
   cat("  gap trials only\n");
   print(summary(aov(rt.all ~ target * soa + Error(sub / (target * soa)),
                     data01[data01$gapdur == "10",])));
}

do.an0101();
rm(do.an0101);
