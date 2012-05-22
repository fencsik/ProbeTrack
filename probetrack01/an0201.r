### an0201: anova on d' as a function of probe delay

do.an0201 <- function () {
   infile <- "data02.rda";
   outfile <- "an0201.txt";

   exit.function <- function () {
      while (sink.number() > 0) sink();
   }
   on.exit(exit.function());

   load(infile);

   data02$gapdur <- factor(data02$gapdur)
   data02$soa <- factor(data02$soa)

   sink(outfile);
   cat("ANOVA on d' as a function of probe delay\n");
   cat("  gap trials only\n");
   print(summary(aov(dprime ~ soa + Error(sub / soa),
                     data02[data02$gapdur == "10",])));
}

do.an0201();
rm(do.an0201);
