### an0201: anova on d' as a function of probe delay
###
### $LastChangedDate$

do.an0201 <- function () {
   thisfile <- "an0201.r";
   infile <- "data02.rda";
   outfile <- "an0201.txt";

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
   cat("ANOVA on d' as a function of probe delay and gap duration\n");
   cat("  gap trials only\n");
   print(summary(aov(dprime ~ soa * gapdur + Error(sub / (soa * gapdur)),
                     data02[data02$gapdur != "0",])));
}

do.an0201();
rm(do.an0201);
