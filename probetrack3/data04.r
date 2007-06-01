### data04.r: fit weibull cdf to RT by probe delay functions from data03
###
### $LastChangedDate$

do.data04 <- function () {
   thisfile <- "data04.r";
   infile <- "data03.rda";
   outfile <- "data04.rda";

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

   ## set-up data matrix
   data03$sub <- as.character(data03$sub);
   data03$gapdur <- as.numeric(as.character(data03$gapdur));
   data03$ntargets <- as.numeric(as.character(data03$ntargets));
   data03$soa <- as.numeric(as.character(data03$soa));
   data03 <- data03[data03$gapdur > 0, ];
   rownames(data03) <- seq_len(dim(data03)[1]);
   data <- data03;
   data$rt.pred <- numeric(length(data$rt));

   ## extract IVs
   Subjects <- as.character(sort(unique(data$sub)));
   GapDurations <- as.character(sort(unique(data$gapdur)));
   NTargets <- as.character(sort(unique(data$ntargets)));

   ## generate table of IVs, collapsing over SOA
   ivs <- with(data,
               aggregate(soa, list(sub=sub, gapdur=gapdur, ntargets=ntargets), min))[, 1:3];

   ## set-up other matrix for storing fit information
   fit <- data.frame(ivs, matrix(NA, nrow = nrow(ivs), ncol = 10,
                                 dimnames = list(as.character(1:nrow(ivs)),
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

   p0 <- c(5, 50, 0);
   names(p0) <- c("slope", "threshold", "baseline");

   for (sub in Subjects) {
      for (gd in GapDurations) {
         for (nt in NTargets) {
            data.index <- data$sub == sub & data$gapdur == gd & data$ntargets == nt;
            fit.index <- fit$sub == sub & fit$gapdur == gd & fit$ntargets == nt;
            x <- data[data.index, "soa"];
            y <- data[data.index, "rt"];
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

   data04 <- list(data = data, fit = data.frame(fit), weibull = weibull);
   save(data04, file=outfile);
}

print(system.time(do.data04(), TRUE));
rm(do.data04);
