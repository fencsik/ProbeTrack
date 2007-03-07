### fig0501.r: plot fit of weibull against observed data for each subject
###
### $LastChangedDate$

do.fig0501 <- function () {
   infile <- "data05.rda";
   outfile <- "fig0501.pdf";
   thisfile <- "fig0501.r";
   exit.function <- function () {
      if (exists("opar")) par(opar);
      if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off();
   }
   on.exit(exit.function());

   if (!file.exists(infile)) stop("cannot open input file ", infile);
   if (IsFileUpToDate(outfile, c(infile, thisfile))) {
      warning("Output file is up to date, no action taken");
      return(invisible(NULL));
   }
   load(infile);
   weibull <- data05$weibull;
   fit <- data05$fit;
   dt <- with(data05$data, tapply(rt.cor, list(soa, sub, gapdur, ntargets), mean));

   x <- as.numeric(dimnames(dt)[[1]]);
   subList <- dimnames(dt)[[2]];
   gapdurList <- dimnames(dt)[[3]];
   ntargetsList <- dimnames(dt)[[4]];

   pdf(outfile, width = 18, height = 8, pointsize = 12);
   opar <- par(mfrow = c(2, 4), las = 1, pty = "m", cex.axis = .6,
               xpd = NA, bg = "white");

   ## set up color matrix
   col <- as.character(with(data05$data, tapply(rt.cor, list(gapdur, ntargets), mean)));
   col <- matrix(rainbow(length(gapdurList) * length(ntargetsList)),
                 nrow = length(gapdurList), ncol = length(ntargetsList),
                 dimnames = list(gapdurList, ntargetsList));
   ## other settings
   ylim.range <- 300;

   counter <- 0;
   for (sub in subList) {
      ylim <- c((mid <- mean(dt[, sub, , ])) - ylim.range/2, mid + ylim.range/2);
      plot(x, dt[, sub, , ], type = "n", bty = "n",
           axes = F, ylim = ylim,
           xlab = "", ylab = "", main = paste("ProbeTrack2", sub));
      axis(1, x, x * 1000 / 75);
      axis(2);
      if (counter %% 8 >= 3) {
         title(xlab = "Probe delay (ms)");
      }
      if (counter %% 4 == 0) {
         title(ylab = "Probe RT (ms)");
      }
      for (gd in gapdurList) {
         for (nt in ntargetsList) {
            index <- fit$sub == sub & fit$gapdur == gd & fit$ntargets == nt;
            lines(i <- seq(0, 100, by = .1),
                  weibull(i, fit[index, "slope"], fit[index, "threshold"],
                          fit[index, "baseline"], fit[index, "asymptote"]),
                          lty = 1, lwd = 3, col = col[gd, nt]);
            points(x, dt[, sub, gd, nt],
                   col = col[gd, nt], bg = "white", pch = 21, cex = 1.5, lwd = 3);
            counter <- counter + 1;
         }
      }
   }
}

do.fig0501();
rm(do.fig0501);
