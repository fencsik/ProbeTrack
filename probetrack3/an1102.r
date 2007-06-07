### an1102: anova to test differences among parameters as a function of gap
### duration
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
   cat("ANOVA testing effect of gap duration on rtime\n");
   print(summary(aov(rtime ~ gapdur + Error(sub / gapdur),
                     data = dt)));

   gapdurList <- sort(unique(as.numeric(as.character(dt$gapdur))));
   nGapdur <- length(gapdurList);
   for (i in 1:(nGapdur-1)) {
      for (j in (i+1):nGapdur) {
         cat("\n\n\n");
         cat(sprintf("ANOVA comparing gap durations %0.0f and %0.0f\n",
                     gapdurList[i], gapdurList[j]));
         print(summary(aov(rtime ~ gapdur + Error(sub / gapdur),
                           data = dt[dt$gapdur == gapdurList[i] | dt$gapdur == gapdurList[j], ])));
      }
   }

   cat("\n\n\n");
   cat("ANOVA testing effect of gap duration on baseRT\n");
   print(summary(aov(baseRT ~ gapdur + Error(sub / gapdur),
                     data = dt)));

   for (i in 1:(nGapdur-1)) {
      for (j in (i+1):nGapdur) {
         cat("\n\n\n");
         cat(sprintf("ANOVA comparing gap durations %0.0f and %0.0f\n",
                     gapdurList[i], gapdurList[j]));
         print(summary(aov(baseRT ~ gapdur + Error(sub / gapdur),
                           data = dt[dt$gapdur == gapdurList[i] | dt$gapdur == gapdurList[j], ])));
      }
   }

}

do.an1102();
rm(do.an1102);
