### an1001: anova on median correct RT as a function of probe delay and gap
### duration
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

   data10$gapdur <- factor(data10$gapdur)
   data10$soa <- factor(data10$soa)

   sink(outfile);
   cat("ANOVA on median correct RT as a function of probe delay and gap duration\n");
   cat("  gap trials only\n");
   print(summary(aov(rt ~ soa * gapdur + Error(sub / (soa * gapdur)),
                     data10[data10$gapdur != "0",])));

   for (gd in levels(data10$gapdur)) {
      if (gd == "0") next;
      cat("\n\n\n");
      cat(sprintf("ANOVA on RT as a function of probe delay at gap duration %s\n", gd));
      print(summary(aov(rt ~ soa + Error(sub / (soa)),
                        data10[data10$gapdur == gd,])));
   }

   for (s in as.character(sort(as.numeric(levels(data10$soa))))) {
      cat("\n\n\n");
      cat(sprintf("ANOVA on RT as a function of gap duration at SOA = %s\n", s));
      print(summary(aov(rt ~ gapdur + Error(sub / (gapdur)),
                        data10[data10$gapdur != "0" & data10$soa == s,])));
   }
}

do.an1001();
rm(do.an1001);
