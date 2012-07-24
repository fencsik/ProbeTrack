### an1401: generate table of parameters of fitted model for each subject
###
### $LastChangedDate$

do.an1401 <- function () {
   thisfile <- "an1401.r";
   infile <- "data14.rda";
   outfile <- "an1401.txt";

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

   dt <- data14$fit;
   dt$r.sq <- dt$r^2;
   category <- c(rownames(dt), "MEAN");
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
   obsey <- with(data14$data, aggregate(rt, list(soa), mean))$x;
   predy <- with(data14$data, aggregate(rt.pred, list(soa), mean))$x;
   cat(sprintf("R-squared = %0.5f\n", cor(obsey, predy) ^ 2));

   cat("\n\n\n");
   cat("Goodness of fit statistics for fit from averaged parameters\n");
   x <- sort(unique(as.numeric(data14$data$soa)));
   predy <- data14$model(x, dt["MEAN", "rtime"], dt["MEAN", "baseRT"]);
   cat(sprintf("R-squared = %0.5f\n", cor(obsey, predy) ^ 2));
}

do.an1401();
rm(do.an1401);
