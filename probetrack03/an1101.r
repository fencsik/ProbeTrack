### an1101: generate table of parameters of fitted model for each subject
###
### $LastChangedDate$

do.an1101 <- function () {
   thisfile <- "an1101.r";
   infile <- "data11.rda";
   outfile <- "an1101.txt";

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
   dt$sub <- as.character(dt$sub);
   dt$gapdur <- as.numeric(as.character(dt$gapdur));
   dt$ntargets <- as.numeric(as.character(dt$ntargets));
   dt$r.sq <- dt$r^2;
   category <- c(with(dt, sprintf("%s-gap%02d-%1dtarg", sub, gapdur, ntargets)),
                 "MEAN");
   dt <- dt[, -1];
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
   y <- with(dt[rownames(dt) != "MEAN", ], tapply(rtime, list(gapdur), mean));
   ci <- with(dt[rownames(dt) != "MEAN", ], tapply(rtime, list(gapdur),
                         function(x) qt(.975, length(x) - 1) * sqrt(var(x) / length(x))));
   for (gd in dimnames(y)[[1]]) {
      cat(sprintf("Gap %3s: %0.2f +/- %0.2f (%0.2f, %0.2f)\n", gd, y[gd], ci[gd],
                  y[gd] - ci[gd], y[gd] + ci[gd]));
   }
   y <- dt$rtime[rownames(dt) != "MEAN"];
   ci <- qt(.975, length(y) - 1) * sqrt(var(y) / length(y));
   cat(sprintf("   MEAN: %0.2f +/- %0.2f (%0.2f, %0.2f)\n", mean(y), ci,
               mean(y) - ci, mean(y) + ci));

   cat("\n\n\n");
   cat("Average baseRT, with 95% confidence intervals\n");
   y <- with(dt[rownames(dt) != "MEAN", ], tapply(baseRT, list(gapdur), mean));
   ci <- with(dt[rownames(dt) != "MEAN", ], tapply(baseRT, list(gapdur),
                         function(x) qt(.975, length(x) - 1) * sqrt(var(x) / length(x))));
   for (gd in dimnames(y)[[1]]) {
      cat(sprintf("Gap %3s: %0.2f +/- %0.2f (%0.2f, %0.2f)\n", gd, y[gd], ci[gd],
                  y[gd] - ci[gd], y[gd] + ci[gd]));
   }
   y <- dt$baseRT[rownames(dt) != "MEAN"];
   ci <- qt(.975, length(y) - 1) * sqrt(var(y) / length(y));
   cat(sprintf("   MEAN: %0.2f +/- %0.2f (%0.2f, %0.2f)\n", mean(y), ci,
               mean(y) - ci, mean(y) + ci));

   cat("\n\n\n");
   cat("Goodness of fit statistics for averaged fit\n");
   obsey <- with(data11$data, aggregate(rt, list(soa, ntargets, gapdur), mean))$x;
   predy <- with(data11$data, aggregate(rt.pred, list(soa, ntargets, gapdur), mean))$x;
   cat(sprintf("R-squared = %0.5f\n", cor(obsey, predy) ^ 2));

   if (length(unique(data11$data$ntargets)) > 1)
      cat("\n\n\nNOT SET UP FOR MULTIPLE LEVELS OF GAPDUR OR NTARGETS\n");
   cat("\n\n\n");
   cat("Goodness of fit statistics for fit from averaged parameters\n");
   m.rtime <- with(dt[rownames(dt) != "MEAN", ], tapply(rtime, list(gapdur), mean));
   m.baseRT <- with(dt[rownames(dt) != "MEAN", ], tapply(baseRT, list(gapdur), mean));
   obse <- with(data11$data, tapply(rt, list(soa, gapdur), mean));
   pred <- with(data11$data, tapply(rt.pred, list(soa, gapdur), mean));
   x <- as.numeric(dimnames(obse)[[1]]);
   for (gd in dimnames(m.rtime)[[1]]) {
      pred[, gd] <- data11$model(x, m.rtime[gd], m.baseRT[gd]);
   }
   cat(sprintf("R-squared = %0.5f\n", cor(as.vector(obse), as.vector(pred)) ^ 2));
}

do.an1101();
rm(do.an1101);
