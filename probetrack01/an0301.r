### an0301: anova on RT as a function of probe delay

do.an0301 <- function () {
   infile <- "data03.rda";
   outfile <- "an0301.txt";

   exit.function <- function () {
      while (sink.number() > 0) sink();
   }
   on.exit(exit.function());

   load(infile);

   sink(outfile);
   cat("ANOVA on correct RT as a function of probe delay\n");
   cat("  gap trials only\n");
   print(summary(aov(rt ~ soa + Error(sub / soa),
                     data03[data03$gapdur == "10",])));

   cat("\n\n\n");
   cat("ANOVA on all RT as a function of probe delay\n");
   cat("  gap trials only\n");
   print(summary(aov(rt.all ~ soa + Error(sub / soa),
                     data03[data03$gapdur == "10",])));
}

do.an0301();
rm(do.an0301);
