### an1102: anova to test differences among parameters as a function of tracking
### load
###
### $LastChangedDate$

do.an1102 <- function () {
   thisfile <- "an1102.r";
   infile <- "data11.rda";
   outfile <- "an1102.txt";

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

   dt <- data11$fit;
   ##dt$sub <- as.character(dt$sub);
   ##dt$gapdur <- as.numeric(as.character(dt$gapdur));
   ##dt$ntargets <- as.numeric(as.character(dt$ntargets));

   sink(outfile);
   cat("ANOVA testing effect of tracking load on rtime\n");
   print(summary(aov(rtime ~ ntargets + Error(sub / ntargets),
                     data = dt)));

   ntargetsList <- sort(unique(as.numeric(as.character(dt$ntargets))));
   nNtargets <- length(ntargetsList);
   for (i in 1:(nNtargets-1)) {
      for (j in (i+1):nNtargets) {
         cat("\n\n\n");
         cat(sprintf("ANOVA comparing tracking loads of %0.0f and %0.0f\n",
                     ntargetsList[i], ntargetsList[j]));
         print(summary(aov(rtime ~ ntargets + Error(sub / ntargets),
                           data = dt[dt$ntargets == ntargetsList[i] |
                             dt$ntargets == ntargetsList[j], ])));
      }
   }

   cat("\n\n\n");
   cat("ANOVA testing effect of tracking load on baseRT\n");
   print(summary(aov(baseRT ~ ntargets + Error(sub / ntargets),
                     data = dt)));

   for (i in 1:(nNtargets-1)) {
      for (j in (i+1):nNtargets) {
         cat("\n\n\n");
         cat(sprintf("ANOVA comparing tracking loads of %0.0f and %0.0f\n",
                     ntargetsList[i], ntargetsList[j]));
         print(summary(aov(baseRT ~ ntargets + Error(sub / ntargets),
                           data = dt[dt$ntargets == ntargetsList[i] |
                             dt$ntargets == ntargetsList[j], ])));
      }
   }

}

do.an1102();
rm(do.an1102);
