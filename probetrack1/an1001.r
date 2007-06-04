### an1001: anova on RT as a function of probe delay
###
### $LastChangedDate$

do.an1001 <- function () {
   thisfile <- "an1001.r";
   infile <- "data10.rda";
   outfile <- "an1001.txt";

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
   cat("ANOVA on median correct RT as a function of probe delay\n");
   cat("  gap trials only\n");
   print(summary(aov(rt ~ soa + Error(sub / soa),
                     data10[data10$gapdur == "10",])));
}

do.an1001();
rm(do.an1001);
