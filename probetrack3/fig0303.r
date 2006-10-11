### fig0303.r: plot correct RT by probe delay separated by gap duration,
### separately for each subject
###
### $LastChangedDate$

do.fig0303 <- function () {
   infile <- "data03.rda";
   outfile <- "fig0303.pdf";
   thisfile <- "fig0303.r";
   exit.function <- function () {
      if (exists("opar")) par(opar);
      if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off();
   }
   on.exit(exit.function());

   if (IsFileUpToDate(outfile, c(infile, thisfile))) {
      warning("Output file is up to date, no action taken");
      return(invisible(NULL));
   }
   load(infile);
   data03$soa <- as.numeric(as.character(data03$soa));
   data03$gapdur <- as.numeric(as.character(data03$gapdur));

   ## extract relevant data
   dtg <- with(data03[data03$gapdur != "0", ],
               tapply(rt.cor, list(soa, sub, gapdur, ntargets), mean, na.rm = TRUE));
   dtng <- with(data03[data03$gapdur == "0",], tapply(rt.cor, list(sub, ntargets), mean));

   x <- as.numeric(dimnames(dtg)[[1]]);
   subList <- dimnames(dtg)[[2]];
   gapdurList <- dimnames(dtg)[[3]];
   ntargetsList <- dimnames(dtg)[[4]];

   ## settings
   col <- matrix(rainbow(length(gapdurList) * length(ntargetsList)),
                 nrow = length(gapdurList), ncol = length(ntargetsList),
                 dimnames = list(gapdurList, ntargetsList));
   pch <- matrix(c(21, 22, 23, 24, 16, 15, 17, 18)[1:(length(gapdurList) * length(ntargetsList))],
                 nrow = length(gapdurList), ncol = length(ntargetsList),
                 dimnames = list(gapdurList, ntargetsList));
   ylim.range <- 300;

   pdf(outfile, width = 9.5, height = 7, pointsize = 12);
   opar <- par(mfrow = c(2, 2), las = 1, pty = "m", cex.axis = .6,
               xpd = NA, bg = "white");

   counter <- 0;
   for (sub in subList) {
      for (nt in ntargetsList) {
         ylim <- c((mid <- mean(dtg[, sub, , ])) - ylim.range/2, mid + ylim.range/2);
         matplot(x, dtg[, sub, , ], type = "n", bty = "n",
                 ylim = ylim, axes = F,
                 xlab = "", ylab = "", main = sprintf("ProbeTrack3 %s", sub));

         axis(1, x, x * 1000 / 75);
         axis(2);
         if (counter %% 4 >= 2) title(xlab = "Probe delay (ms)");
         if (counter %% 2 == 0) title(ylab = "Probe RT (ms)");

         lines(x, rep(dtng[sub, nt], length(x)), type = "l",
               col = 1, lty = 2, lwd = 3);

         for (gd in gapdurList) {
            lines(x, dtg[, sub, gd, nt], type = "o",
                  col = col[gd, nt], pch = pch[gd, nt], lty = 1, lwd = 3, cex = 1.5, bg = "white");
         }
         if (counter %% 4 == 0) {
            legend(min(x) + xinch(2), min(ylim) - yinch(.6),
                   sprintf("Gap %s", c("0", gapdurList)),
                   lty = c(2, rep(1, length(gapdurList))), lwd = 2, col = c(1, col[gapdurList,]),
                   bty = "n", ncol = length(gapdurList) + 1, y.intersp = 1.3);
         }
         counter <- counter + 1;
      }
   }
}

do.fig0303();
rm(do.fig0303);
