### an1001: anova on median correct RT as a function of probe delay and the
### number of targets
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

   data10 <- data10[data10$gapdur != "0", ];
   data10$gapdur <- factor(data10$gapdur);

   sink(outfile);
   cat("ANOVA on median correct RT as a function of probe delay and tracking load\n");
   cat("  gap trials only\n");
   print(summary(aov(rt ~ soa * ntargets + Error(sub / (soa * ntargets)),
                     data10[data10$ntargets != "0",])));

   for (nt in levels(data10$ntargets)) {
      if (nt == "0") next;
      cat("\n\n\n");
      cat(sprintf("ANOVA on RT as a function of probe delay with %s target(s)\n", nt));
      print(summary(aov(rt ~ soa + Error(sub / (soa)),
                        data10[data10$ntargets == nt,])));
   }

   for (s in as.character(sort(as.numeric(levels(data10$soa))))) {
      cat("\n\n\n");
      cat(sprintf("ANOVA on RT as a function of tracking load at SOA = %s\n", s));
      print(summary(aov(rt ~ ntargets + Error(sub / (ntargets)),
                        data10[data10$soa == s,])));
   }

   cat("\n\n\n")
   cat("ANOVA on RT as a function of tracking load and SOA\n")
   cat("  at tracking loads > 1 and SOA = 0 and 1280 ms\n")
   print(summary(aov(rt ~ soa * ntargets + Error(sub / (soa * ntargets)),
                     data10[(data10$soa == "0" | data10$soa == "1280") &
                            data10$ntargets != "1", ])))
}

do.an1001();
rm(do.an1001);
