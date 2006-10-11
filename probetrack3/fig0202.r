### fig0202.r: plot d' as a function of probe delay, separated by gap duration,
### separately for each subject
###
### $LastChangedDate$

do.fig0202 <- function () {
   infile <- "data02.rda";
   outfile <- "fig0202.pdf";
   thisfile <- "fig0202.r";
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
   data02$soa <- as.numeric(as.character(data02$soa));
   data02$gapdur <- as.numeric(as.character(data02$gapdur));

   ## extract relevant data
   dtg <- with(data02[data02$gapdur != "0", ],
               tapply(dprime, list(soa, sub, gapdur, ntargets), mean, na.rm = TRUE));
   dtng <- with(data02[data02$gapdur == "0",], tapply(dprime, list(sub, ntargets), mean));

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
   ylim.range <- 4;

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
         if (counter %% 2 == 0) title(ylab = "d'");

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

do.fig0202();
rm(do.fig0202);
