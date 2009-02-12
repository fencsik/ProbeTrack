### an1002: ancova on median correct RT as a function of probe delay and
### tracking load (tracking load is entered as numeric, instead of a factor, so
### is treated as a ratio variable instead of categorical and uses only 1 df)
###
### $LastChangedDate$

do.an1002 <- function () {
   thisfile <- "an1002.r";
   infile <- "data10.rda";
   outfile <- "an1002.txt";

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

   data10 <- data10[data10$gapdur != "0", ];
   data10$ntargets <- as.numeric(as.character(data10$ntargets));

   sink(outfile);
   cat("ANOVA on median correct RT as a function of probe delay and tracking load\n");
   cat("  gap trials only\n");
   print(summary(aov(rt ~ soa * ntargets + Error(sub / (soa * ntargets)),
                     data10[data10$ntargets != "0",])));

   for (s in as.character(sort(as.numeric(levels(data10$soa))))) {
      cat("\n\n\n");
      cat(sprintf("ANOVA on RT as a function of tracking load at SOA = %s\n", s));
      print(summary(aov(rt ~ ntargets + Error(sub / (ntargets)),
                        data10[data10$soa == s,])));
   }
}

do.an1002();
rm(do.an1002);
