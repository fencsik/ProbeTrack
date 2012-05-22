### an1001: anova on RT as a function of probe delay

do.an1001 <- function () {
   infile <- "data10.rda";
   outfile <- "an1001.txt";

   exit.function <- function () {
      while (sink.number() > 0) sink();
   }
   on.exit(exit.function());

   load(infile);

   data10$gapdur <- factor(data10$gapdur)
   data10$soa <- factor(data10$soa)

   sink(outfile);
   cat("ANOVA on median correct RT as a function of probe delay\n");
   cat("  gap trials only\n");
   print(summary(aov(rt ~ soa + Error(sub / soa),
                     data10[data10$gapdur == "10",])));
}

do.an1001();
rm(do.an1001);
