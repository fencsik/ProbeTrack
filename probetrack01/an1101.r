### an1101: generate table of parameters of fitted model for each subject

do.an1101 <- function () {
   infile <- "data11.rda";
   outfile <- "an1101.txt";

   exit.function <- function () {
      while (sink.number() > 0) sink();
   }
   on.exit(exit.function());

   load(infile);

   dt <- data11$fit;
   dt$sub <- as.character(dt$sub);
   dt$gapdur <- as.numeric(as.character(dt$gapdur));
   dt$ntargets <- as.numeric(as.character(dt$ntargets));
   dt$r.sq <- dt$r^2;
   category <- c(with(dt, sprintf("%s-gap%02d-%1dtarg", sub, gapdur, ntargets)),
                 "MEAN");
   dt <- as.matrix(dt[, -1]);
   dt <- rbind(dt, apply(as.matrix(dt), 2, mean));
   rownames(dt) <- category;

   sink(outfile);
   cat("Parameter estimates from model fit\n");
   print(round(dt[, c("rtime", "baseRT")], 3));

   cat("\n\n\n");
   cat("Fitting output and goodness-of-fit statistics\n");
   print(round(dt[, c("iter", "code", "sse", "rmse", "r.sq", "chisq")], 3));

   cat("\n\n\n");
   cat("Average rtime, with 95% confidence intervals\n");
   y <- dt[rownames(dt) != "MEAN", "rtime"];
   ci <- qt(.975, length(y) - 1) * sqrt(var(y) / length(y));
   cat(sprintf("%0.2f +/- %0.2f (%0.2f, %0.2f)\n", mean(y), ci,
                 mean(y) - ci, mean(y) + ci));

   cat("\n\n\n");
   cat("Average baseRT, with 95% confidence intervals\n");
   y <- dt[rownames(dt) != "MEAN", "baseRT"];
   ci <- qt(.975, length(y) - 1) * sqrt(var(y) / length(y));
   cat(sprintf("%0.2f +/- %0.2f (%0.2f, %0.2f)\n", mean(y), ci,
                 mean(y) - ci, mean(y) + ci));

   cat("\n\n\n");
   cat("Goodness of fit statistics for averaged fit\n");
   obsey <- with(data11$data, aggregate(rt, list(soa, ntargets, gapdur), mean))$x;
   predy <- with(data11$data, aggregate(rt.pred, list(soa, ntargets, gapdur), mean))$x;
   cat(sprintf("R-squared = %0.5f\n", cor(obsey, predy) ^ 2));

   if (length(unique(data11$data$gapdur)) > 1 || length(unique(data11$data$ntargets)) > 1)
      cat("\n\n\nNOT SET UP FOR MULTIPLE LEVELS OF GAPDUR OR NTARGETS\n");
   cat("\n\n\n");
   cat("Goodness of fit statistics for fit from averaged parameters\n");
   x <- sort(unique(as.numeric(data11$data$soa)));
   predy <- data11$model(x, dt["MEAN", "rtime"], dt["MEAN", "baseRT"]);
   cat(sprintf("R-squared = %0.5f\n", cor(obsey, predy) ^ 2));
}

do.an1101();
rm(do.an1101);
