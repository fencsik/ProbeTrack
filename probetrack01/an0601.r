### an0601: generate table of parameters of weibull for each subject

do.an0601 <- function () {
   infile <- "data06.rda";
   outfile <- "an0601.txt";

   exit.function <- function () {
      while (sink.number() > 0) sink();
   }
   on.exit(exit.function());

   load(infile);

   dt <- data06$fit;
   dt$sub <- as.character(dt$sub);
   dt$gapdur <- as.numeric(as.character(dt$gapdur));
   dt$ntargets <- as.numeric(as.character(dt$ntargets));
   dt$r.sq <- dt$r^2;
   category <- with(dt, sprintf("%s-gap%02d-%1dtarg", sub, gapdur, ntargets));

   sink(outfile);
   cat("Parameter estimates from weibull fit\n");
   print(cbind(category, round(dt[, c("slope", "threshold", "baseline", "asymptote")], 3)));

   cat("\n\n\n");
   cat("Fitting output and goodness-of-fit statistics\n");
   print(cbind(category, round(dt[, c("iter", "code", "sse", "rmse", "r.sq", "chisq")], 3)));
}

do.an0601();
rm(do.an0601);
