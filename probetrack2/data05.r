### data05.r: fit weibull cdf to RT by probe delay functions from data01
###
### $LastChangedDate$

do.data05 <- function () {
   thisfile <- "data05.r";
   infile <- "data01.rda";
   outfile <- "data05.rda";

   exit.function <- function () {
      if (exists("old.opt")) options(old.opt);
   }
   on.exit(exit.function());
   old.opt <- options(warn = 1);

   if (!file.exists(infile)) stop("cannot open input file ", infile);
   if (IsFileUpToDate(outfile, c(thisfile, infile))) {
      warning("Output file is up to date, no action taken");
      return(invisible(NULL));
   }
   load(infile);

   ## optionally remove ssf because the weibull fit is weird
   ##data01 <- data01[data01$sub != "ssf", ];

   ## set-up data matrix
   data01$sub <- as.character(data01$sub);
   data01$gapdur <- as.numeric(as.character(data01$gapdur));
   data01$target <- as.numeric(as.character(data01$target));
   data01$ntargets <- as.numeric(as.character(data01$ntargets));
   data01$soa <- as.numeric(as.character(data01$soa));
   data01 <- data01[data01$gapdur > 0 & data01$target == 1, ];
   data <- data01;
   data$rt.pred <- numeric(length(data$rt.cor));

   ## extract IVs
   Subjects <- as.character(sort(unique(data$sub)));
   GapDurations <- as.character(sort(unique(data$gapdur)));
   NTargets <- as.character(sort(unique(data$ntargets)));

   ## generate table of IVs, collapsing over SOA
   ivs <- with(data,
               aggregate(soa, list(sub=sub, gapdur=gapdur, ntargets=ntargets), min))[, 1:3];

   ## set-up other matrix for storing fit information
   fit <- data.frame(ivs, matrix(NA, nrow = nrow(ivs), ncol = 10,
                                 dimnames = list(Subjects,
                                   c("slope", "threshold", "baseline", "asymptote",
                                     "iter", "code", "sse", "rmse", "r", "chisq"))));
   fit.columns <- 4:ncol(fit);

   weibull <- function(x, slope, threshold, baseline, asymptote) {
      baseline + (asymptote - baseline) *
         (1 - pweibull(x - min(x), shape = slope, scale = threshold));
   }

   GoodnessOfFit <- function(p, asymptote) {
      ## parameters: (1) slope, (2) threshold, (3) baseline
      if (p[1] > 0 && p[2] > 0 && abs(p[3] - min(y)) < min(y) / 2) {
         sum( (y - weibull(x, slope = p[1], threshol = p[2], baseline = p[3], asymptote)) ^ 2 );
      } else {
         10^12;
      }
   }

   p0 <- c(5, 5, 0);
   names(p0) <- c("slope", "threshold", "baseline");

   for (sub in Subjects) {
      for (gd in GapDurations) {
         for (nt in NTargets) {
            data.index <- data$sub == sub & data$gapdur == gd & data$ntargets == nt;
            fit.index <- fit$sub == sub & fit$gapdur == gd & fit$ntargets == nt;
            x <- data[data.index, "soa"];
            y <- data[data.index, "rt.cor"];
            p0["baseline"] <- min(y);
            asymptote <- max(y);
            out <- nlm(GoodnessOfFit, p0, print.level = 0, asymptote = asymptote);
            p <- out$estimate;
            yhat <- weibull(x, p[1], p[2], p[3], asymptote);
            data[data.index, "rt.pred"] <- yhat;
            fit[fit.index, fit.columns] <- c(p, asymptote, out$iterations, out$code,
                                             GoodnessOfFit(p, asymptote),
                                             sqrt(mean((y - yhat)^2)),
                                             cor(y, yhat),
                                             sum((y - yhat) ^ 2 / yhat));
         }
      }
   }

   data05 <- list(data = data, fit = data.frame(fit), weibull = weibull);
   save(data05, file=outfile);
}

print(system.time(do.data05(), TRUE));
rm(do.data05);
