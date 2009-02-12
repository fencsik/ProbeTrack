### data07.r: fit weibull cdf to RT by probe delay function for averaged data
### from data01, separately for target- and distractor-probe trials
###
### $LastChangedDate$

do.data07 <- function () {
   thisfile <- "data07.r";
   infile <- "data01.rda";
   outfile <- "data07.rda";

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

   ## optionally remove el and ssf because of low accuracy
   ##data01 <- data01[data01$sub != "ssf" & data01$sub != "el", ];

   ## set-up data matrix
   data01$sub <- as.character(data01$sub);
   data01$gapdur <- as.numeric(as.character(data01$gapdur));
   data01$ntargets <- as.numeric(as.character(data01$ntargets));
   data01$soa <- as.numeric(as.character(data01$soa));
   data01 <- data01[data01$gapdur > 0, ];

   ## collapse across subjects
   data <- with(data01, aggregate(data.frame(rt = rt),
                                  list(soa = soa, gapdur = gapdur,
                                       ntargets = ntargets, target = target), mean));
   data$rt.pred <- numeric(length(data$rt));
   data$soa <- as.numeric(as.character(data$soa));
   data$gapdur <- as.numeric(as.character(data$gapdur));
   data$ntargets <- as.numeric(as.character(data$ntargets));
   data$target <- as.numeric(as.character(data$target));

   ## generate table of IVs, collapsing over SOA
   ivs <- with(data,
               aggregate(soa, list(gapdur=gapdur, ntargets=ntargets, target=target), min))[, 1:3];

   ## set-up other matrix for storing fit information
   fit <- data.frame(ivs, matrix(NA, nrow = nrow(ivs), ncol = 10,
                                 dimnames = list(NULL,
                                   c("slope", "threshold", "baseline", "asymptote",
                                     "iter", "code", "sse", "rmse", "r", "chisq"))));
   fit.columns <- 4:ncol(fit);

   weibull <- function(x, slope, threshold, baseline, asymptote) {
      baseline + (asymptote - baseline) *
         (1 - pweibull(x, shape = slope, scale = threshold));
   }

   GoodnessOfFit <- function(p, asymptote) {
      ## parameters: (1) slope, (2) threshold, (3) baseline
      ##if (p[1] > 0 && p[2] > 0 && abs(p[3] - min(y)) < min(y) / 2) {
      if (p[1] > 0 && p[2] > 0) {
         sum( (y - weibull(x, slope = p[1], threshold = p[2], baseline = p[3], asymptote)) ^ 2 );
      } else {
         10^12;
      }
   }

   p0 <- c(10, 10, 0);
   names(p0) <- c("slope", "threshold", "baseline");

   for (gd in sort(unique(as.character(data$gapdur)))) {
      for (nt in sort(unique(as.character(data$ntargets)))) {
         for (targ in sort(unique(as.character(data$target)))) {
            index <- data$gapdur == gd & data$ntargets == nt & data$target == targ;
            x <- data[index, "soa"];
            y <- data[index, "rt"];
            p0["baseline"] <- min(y);
            asymptote <- max(y);
            out <- nlm(GoodnessOfFit, p0, print.level = 0, asymptote = asymptote);
            p <- out$estimate;
            yhat <- weibull(x, p[1], p[2], p[3], asymptote);
            data[index, "rt.pred"] <- yhat;

            index <- fit$gapdur == gd & fit$ntargets == nt & fit$target == targ;
            fit[index, fit.columns] <- c(p, asymptote, out$iterations, out$code,
                                             GoodnessOfFit(p, asymptote),
                                             sqrt(mean((y - yhat)^2)),
                                             cor(y, yhat),
                                             sum((y - yhat) ^ 2 / yhat));
         }
      }
   }

   data07 <- list(data = data, fit = data.frame(fit), weibull = weibull);
   save(data07, file=outfile);
}

print(system.time(do.data07(), TRUE));
rm(do.data07);
