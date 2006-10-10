### fig0401.r: plot fit of weibull against observed data for each subject
###
### $LastChangedDate$

do.fig0401 <- function () {
   infile <- "data04.rda";
   outfile <- "fig0401.pdf";
   thisfile <- "fig0401.r";
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
   weibull <- data04$weibull;
   fit <- data04$fit;
   dt <- with(data04$data, tapply(rt.cor, list(soa, sub, gapdur, ntargets), mean));

   x <- as.numeric(dimnames(dt)[[1]]);
   subList <- dimnames(dt)[[2]];
   gapdurList <- dimnames(dt)[[3]];
   ntargetsList <- dimnames(dt)[[4]];

   pdf(outfile, width = 9.5, height = 7, pointsize = 12);
   opar <- par(mfrow = c(2, 2), las = 1, pty = "m", cex.axis = .6,
               xpd = NA, bg = "white");

   ## set up color matrix
   col <- as.character(with(data04$data, tapply(rt.cor, list(gapdur, ntargets), mean)));
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
           xlab = "", ylab = "", main = paste("ProbeTrack1", sub));
      axis(1, x, x * 1000 / 75);
      axis(2);
      if (counter %% 4 >= 2) {
         title(xlab = "Probe delay (ms)");
      }
      if (counter %% 2 == 0) {
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

do.fig0401();
rm(do.fig0401);
