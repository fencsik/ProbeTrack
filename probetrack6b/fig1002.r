### fig1002.r: plot correct median RT by probe delay separated by tracking load,
### separately for each subject
###
### $LastChangedDate$

do.fig1002 <- function () {
   infile <- "data10.rda";
   outfile <- "fig1002.pdf";
   thisfile <- "fig1002.r";
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
   data10$sub <- as.character(data10$sub);
   data10$soa <- as.numeric(as.character(data10$soa));
   data10$ntargets <- as.numeric(as.character(data10$ntargets));
   data10$gapdur <- as.numeric(as.character(data10$gapdur));
   data10 <- data10[data10$gapdur > 0, ];

   ## extract relevant data
   dt <- with(data10, tapply(rt, list(soa, ntargets, sub), mean, na.rm = TRUE));
   x <- as.numeric(dimnames(dt)[[1]]);

   ## settings
   ylim <- c(300, 700);
   cond.names <- dimnames(dt)[[2]];
   nCond <- length(cond.names);
   col <- rainbow(nCond);                               names(col) <- cond.names;
   lty <- rep(1, nCond);                                names(lty) <- cond.names;
   pch <- c(21, 22, 23, 24, 16, 15, 17, 18)[1:nCond];   names(pch) <- cond.names;

   pdf(outfile, width = 9.5, height = 7, pointsize = 12);
   opar <- par(mfrow = c(2, 2), las = 1, pty = "m", cex.axis = .6,
               xpd = NA, bg = "white");

   counter <- 0;
   for (sub in dimnames(dt)[[3]]) {
      matplot(x, dt[, , sub], type = "n", bty = "n",
              ylim = ylim, axes = F,
              xlab = "", ylab = "", main = paste("ProbeTrack6b", sub));
      axis(1, x);
      axis(2);
      if (counter %% 2 == 0) title(ylab = "Correct median RT (ms)");
      if (counter %% 4 >= 2) title(xlab = "Probe delay (ms)");

      for (nt in dimnames(dt)[[2]]) {
         lines(x, dt[, nt, sub], type = "o",
               col = col[nt], pch = pch[nt], lty = lty[nt], lwd = 3, cex = 1.5, bg = "white");
      }
      if (counter %% 4 == 0) {
         legend("bottomleft", paste(cond.names, "target(s)     "), inset = c(.65, -.45),
                col = col, pch = pch,
                lty = lty, lwd = 3, pt.bg = "white", pt.cex = 1.5,
                bty = "n", ncol = nCond, y.intersp = 1.3, cex = .8);
      }
      counter <- counter + 1;
   }
}

do.fig1002();
rm(do.fig1002);
