### an0201: anova on d' as a function of probe delay and number of targets
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

   data02 <- data02[data02$gapdur != "0", ];
   data02$gapdur <- factor(data02$gapdur);

   sink(outfile);
   cat("ANOVA on d' as a function of probe delay and number of targets\n");
   cat("  gap trials only\n");
   print(summary(aov(dprime ~ soa * ntargets + Error(sub / (soa * ntargets)),
                     data02)));

   for (nt in levels(data02$ntargets)) {
      cat("\n\n\n");
      cat(sprintf("ANOVA on d' as a function of probe delay with %s target(s)\n", nt));
      print(summary(aov(dprime ~ soa + Error(sub / soa),
                        data02[data02$ntargets == nt,])));
   }

   for (s in as.character(sort(as.numeric(levels(data02$soa))))) {
      cat("\n\n\n");
      cat(sprintf("ANOVA on d' as a function of number of targets at SOA = %s\n", s));
      print(summary(aov(dprime ~ ntargets + Error(sub / ntargets),
                        data02[data02$soa == s,])));
   }
}

do.an0201();
rm(do.an0201);
