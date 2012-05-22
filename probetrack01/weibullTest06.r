### weibullTest06.r: tests fitting of weibull to each subject's data with various
### starting parameters

weibull <- function(x, slope, threshold, baseline, asymptote) {
   baseline + (asymptote - baseline) *
      (1 - pweibull(x - min(x), shape = slope, scale = threshold));
}

plotfit <- function(file, data, fit) {
   exit.function <- function () {
      if (exists("opar")) par(opar);
      if (names(dev.cur()) == "pdf") dev.off();
   }
   on.exit(exit.function());

   dt <- with(data, tapply(rt, list(soa, gapdur,ntargets), mean));
   x <- as.numeric(dimnames(dt)[[1]]);
   gapdurList <- dimnames(dt)[[2]];
   ntargetsList <- dimnames(dt)[[3]];

   pdf(file, width = 6, height = 6, pointsize = 12);
   opar <- par(mfrow = c(1, 1), las = 1, pty = "m", cex.axis = .6,
               xpd = NA, bg = "white");

   ## set up color matrix
   col <- as.character(with(data, tapply(rt, list(gapdur, ntargets), mean)));
   col <- matrix(rainbow(length(gapdurList) * length(ntargetsList)),
                 nrow = length(gapdurList), ncol = length(ntargetsList),
                 dimnames = list(gapdurList, ntargetsList));
   plot(x, dt, type = "n", bty = "n",
        axes = F, ylim = c(min(fit[, "baseline"], data[, "rt"]), max(fit[, "asymptote"])),
        xlab = "Probe delay (ms)", ylab = "Probe RT (ms)", main = "ProbeTrack1");
   axis(1, x);
   axis(2);
   for (gd in gapdurList) {
      for (nt in ntargetsList) {
         index <- fit$gapdur == gd & fit$ntargets == nt;
         lines(i <- seq(min(x), max(x), by = 1),
               weibull(i, fit[index, "slope"], fit[index, "threshold"],
                       fit[index, "baseline"], fit[index, "asymptote"]),
               lty = 1, lwd = 3, col = col[gd, nt]);
         abline(h = fit[index, "baseline"], xpd = F,
                lty = 2, col = col[gd, nt], lwd = 2);
         points(x, dt[, gd, nt],
                col = col[gd, nt], bg = "white", pch = 21, cex = 1.5, lwd = 3);
      }
   }
}

do.weibullTest06 <- function () {
   infile <- "data03.rda";
   outfile <- "weibullTest06.pdf";

   exit.function <- function () {
      if (exists("old.opt")) options(old.opt);
      if (exists("opar")) par(opar);
      if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off();
   }
   on.exit(exit.function());
   old.opt <- options(warn = 1);

   load(infile);

   ## optionally remove ssf and wz because their weibull fits are weird
   ##data03 <- data03[data03$sub != "ssf" & data03$sub != "wz", ];
   data03 <- data03[data03$sub != "ssf", ];

   ## set-up data matrix
   data03$sub <- as.character(data03$sub);
   data03$gapdur <- as.numeric(as.character(data03$gapdur));
   data03$ntargets <- as.numeric(as.character(data03$ntargets));
   data03$soa <- as.numeric(as.character(data03$soa));
   data03 <- data03[data03$gapdur > 0, ];
   data <- with(data03, aggregate(data.frame(rt = rt),
                                  list(soa = soa, gapdur = gapdur, ntargets = ntargets), mean));
   data$soa = as.numeric(as.character(data$soa));
   data$rt.pred <- numeric(length(data$rt));

   ## extract IVs
   GapDurations <- as.character(sort(unique(data$gapdur)));
   NTargets <- as.character(sort(unique(data$ntargets)));

   ## generate table of IVs, collapsing over SOA and subjects
   ivs <- with(data,
               aggregate(soa, list(gapdur=gapdur, ntargets=ntargets), min))[, 1:2];

   ## set-up other matrix for storing fit information
   fit <- data.frame(ivs, matrix(NA, nrow = nrow(ivs), ncol = 10,
                                 dimnames = list(seq_len(nrow(ivs)),
                                   c("slope", "threshold", "baseline", "asymptote",
                                     "iter", "code", "sse", "rmse", "r.sq", "chisq"))));
   fit.columns <- 3:ncol(fit);

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

   for (gd in GapDurations) {
      for (nt in NTargets) {
         data.index <- data$gapdur == gd & data$ntargets == nt;
         fit.index <- fit$gapdur == gd & fit$ntargets == nt;
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
                                          cor(y, yhat) ^ 2,
                                          sum((y - yhat) ^ 2 / yhat));
      }
   }

   cat("Start parameters:\n");
   print(p0);
   cat("End parameters:\n");
   print(fit[, c(1:4, 10:12)]);
   plotfit(outfile, data, fit);
}

print(system.time(do.weibullTest06(), TRUE));
rm(do.weibullTest06);
