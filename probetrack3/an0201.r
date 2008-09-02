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

   data02$gapdur <- factor(data02$gapdur)
   data02$soa <- factor(data02$soa)

   sink(outfile);
   cat("ANOVA on d' as a function of probe delay and gap duration\n");
   cat("  gap trials only\n");
   print(summary(aov(dprime ~ soa * gapdur + Error(sub / (soa * gapdur)),
                     data02[data02$gapdur != "0",])));

   for (gd in levels(data02$gapdur)) {
      if (gd == "0") next;
      cat("\n\n\n");
      cat(sprintf("ANOVA on d' as a function of probe delay at gap duration %s\n", gd));
      print(summary(aov(dprime ~ soa + Error(sub / (soa)),
                        data02[data02$gapdur == gd,])));
   }

   for (s in as.character(sort(as.numeric(levels(data02$soa))))) {
      cat("\n\n\n");
      cat(sprintf("ANOVA on RT as a function of gap duration at SOA = %s\n", s));
      print(summary(aov(dprime ~ gapdur + Error(sub / (gapdur)),
                        data02[data02$gapdur != "0" & data02$soa == s,])));
   }
}

do.an0201();
rm(do.an0201);
