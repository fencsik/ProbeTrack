### an0701: generate table of parameters of fitted model for each subject
###
### $LastChangedDate$

do.an0701 <- function () {
   thisfile <- "an0701.r";
   infile <- "data07.rda";
   outfile <- "an0701.txt";

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

   dt <- data07$fit;
   dt$sub <- as.character(dt$sub);
   dt$gapdur <- as.numeric(as.character(dt$gapdur));
   dt$ntargets <- as.numeric(as.character(dt$ntargets));
   dt$r.sq <- dt$r^2;
   category <- with(dt, sprintf("%s-gap%02d-%1dtarg", sub, gapdur, ntargets));

   sink(outfile);
   cat("Parameter estimates from model fit\n");
   print(cbind(category, round(dt[, c("rtime", "baseRT")], 3)));

   cat("\n\n\n");
   cat("Fitting output and goodness-of-fit statistics\n");
   print(cbind(category, round(dt[, c("iter", "code", "sse", "rmse", "r.sq", "chisq")], 3)));
}

do.an0701();
rm(do.an0701);