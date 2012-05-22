### an0801: generate table of parameters of fitted model for each subject

do.an0801 <- function () {
   infile <- "data08.rda";
   outfile <- "an0801.txt";

   exit.function <- function () {
      while (sink.number() > 0) sink();
   }
   on.exit(exit.function());

   load(infile);

   dt <- data08$fit;
   dt$sub <- as.character(dt$sub);
   dt$gapdur <- as.numeric(as.character(dt$gapdur));
   dt$ntargets <- as.numeric(as.character(dt$ntargets));
   dt$r.sq <- dt$r^2;
   category <- with(dt, sprintf("%s-gap%02d-%1dtarg", sub, gapdur, ntargets));

   sink(outfile);
   cat("Parameter estimates from model fit\n");
   print(cbind(category, round(dt[, c("rtime", "baseRT")], 3)));

   cat("\n\n\n");
   cat("Average rtime, with 95% confidence intervals\n");
   y <- dt[, "rtime"];
   ci <- qt(.975, length(y) - 1) * sqrt(var(y) / length(y));
   cat(sprintf("%0.2f +/- %0.2f (%0.2f, %0.2f)", mean(y), ci,
                 mean(y) - ci, mean(y) + ci));

   cat("\n\n\n");
   cat("Fitting output and goodness-of-fit statistics\n");
   print(cbind(category, round(dt[, c("iter", "code", "sse", "rmse", "r.sq", "chisq")], 3)));
}

do.an0801();
rm(do.an0801);
