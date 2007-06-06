### data11.r: fit reacquire-time model to RT by probe delay functions from
### data10, allowing different reacquisition times for each gap duration
###
### $LastChangedDate$

do.data11 <- function () {
   thisfile <- "data11.r";
   infile <- "data10.rda";
   outfile <- "data11.rda";

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
   data10$sub <- as.character(data10$sub);
   data10$gapdur <- as.numeric(as.character(data10$gapdur));
   data10$ntargets <- as.numeric(as.character(data10$ntargets));
   data10$soa <- as.numeric(as.character(data10$soa));
   data10 <- data10[data10$gapdur > 0, ];
   rownames(data10) <- seq_len(dim(data10)[1]);
   data <- data10;
   data$rt.pred <- numeric(length(data$rt));

   ## extract IVs
   Subjects <- as.character(sort(unique(data$sub)));
   GapDurations <- as.character(sort(unique(data$gapdur)));
   NTargets <- as.character(sort(unique(data$ntargets)));

   ## generate table of IVs, collapsing over SOA
   ivs <- with(data,
               aggregate(soa, list(sub=sub, gapdur=gapdur, ntargets=ntargets), min))[, 1:3];

   ## set-up other matrix for storing fit information
   fit <- data.frame(ivs, matrix(NA, nrow = nrow(ivs), ncol = 8,
                                 dimnames = list(rep(Subjects, length(GapDurations)),
                                   c("rtime", "baseRT",
                                     "iter", "code", "sse", "rmse", "r", "chisq"))));
   fit.columns <- 4:ncol(fit);

   reacquire.model <- function (soa, rtime, baseRT) {
      baseRT + ifelse(rtime - soa > 0, rtime - soa, 0);
   }

   GoodnessOfFit <- function(p) {
      ## parameters: (1) rtime, (2) baseRT
###      if (p[1] > 0 && p[2] > 0 && abs(p[3] - min(y)) < min(y) / 2) {
      sum( (y - reacquire.model(x, rtime = p[1], baseRT = p[2])) ^ 2 );
###      } else {
###         10^12;
###      }
   }

   p0 <- c(40, 500);
   names(p0) <- c("rtime", "baseRT");

   for (sub in Subjects) {
      for (gd in GapDurations) {
         for (nt in NTargets) {
            data.index <- data$sub == sub & data$gapdur == gd & data$ntargets == nt;
            fit.index <- fit$sub == sub & fit$gapdur == gd & fit$ntargets == nt;
            x <- data[data.index, "soa"];
            y <- data[data.index, "rt"];
            p0["baseRT"] <- min(y);
            out <- nlm(GoodnessOfFit, p0, print.level = 0);
            p <- out$estimate;
            yhat <- reacquire.model(x, p[1], p[2]);
            data[data.index, "rt.pred"] <- yhat;
            fit[fit.index, fit.columns] <- c(p, out$iterations, out$code,
                                             GoodnessOfFit(p),
                                             sqrt(mean((y - yhat)^2)),
                                             cor(y, yhat),
                                             sum((y - yhat) ^ 2 / yhat));
         }
      }
   }

   data11 <- list(data = data, fit = data.frame(fit), model = reacquire.model);
   save(data11, file=outfile);
}

print(system.time(do.data11(), TRUE));
rm(do.data11);
