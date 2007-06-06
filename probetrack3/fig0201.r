### fig0201.r: plot d' by probe delay separated by gap duration
###
### $LastChangedDate$

do.fig0201 <- function () {
   infile <- "data02.rda";
   outfile <- "fig0201.pdf";
   thisfile <- "fig0201.r";
   errfile <- "an0202.rda";
   exit.function <- function () {
      if (exists("opar")) par(opar);
      if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off();
   }
   on.exit(exit.function());

   if (!file.exists(infile)) stop("cannot open input file ", infile);
   if (IsFileUpToDate(outfile, c(infile, thisfile, errfile))) {
      warning("Output file is up to date, no action taken");
      return(invisible(NULL));
   }
   load(infile);
   data02$soa <- as.numeric(as.character(data02$soa));
   data02$gapdur <- as.numeric(as.character(data02$gapdur));

   ## extract relevant data
   dt <- with(data02, tapply(dprime, list(soa, gapdur), mean, na.rm = TRUE));
   if (file.exists(errfile)) {
      load(errfile);
      errg <- an0202[, , "ci"];
   } else {
      errg <- with(data02[data02$gapdur != "0", ],
                   tapply(dprime, list(soa, gapdur),
                          function(x) qt(.975, length(x) - 1) * sqrt(var(x, na.rm = TRUE) / length(x))));
   }

   ## fix zero-gap condition
   index <- is.na(dt[, "0"]);
   dt[index, "0"] <- dt[!index, "0"][1];

   x <- as.numeric(dimnames(dt)[[1]]);

   ## settings
   ylim <- c(0, 4);
   cond.names <- dimnames(dt)[[2]];
   nCond <- length(cond.names);
   col <- rainbow(nCond);                               names(col) <- cond.names;
   lty <- c(2, rep(1, nCond - 1));                      names(lty) <- cond.names;
   pch <- c(21, 22, 23, 24, 16, 15, 17, 18)[1:nCond];   names(pch) <- cond.names;
   col["0"] <- "black";
   pch["0"] <- NA;

   pdf(outfile, width = 6, height = 6, pointsize = 12);
   opar <- par(mfrow = c(1, 1), las = 1, pty = "m", cex.axis = .6,
               xpd = NA, bg = "white");

   matplot(x, dt, type = "n", bty = "n",
           ylim = ylim, axes = F,
           xlab = "Probe delay (ms)", ylab = "d'", main = "ProbeTrack3");
   axis(1, x);
   axis(2);

   for (gd in dimnames(dt)[[2]]) {
      if (!is.null(errg) && gd != "0") {
         arrows(x, dt[, gd] - errg[, gd], x, dt[, gd] + errg[, gd],
                length = .05, angle = 90, code = 3, lwd = 1, col = col[gd], lty = 1);
      }
      lines(x, dt[, gd], type = "o",
            col = col[gd], pch = pch[gd], lty = lty[gd], lwd = 3, cex = 1.5, bg = "white");
   }
   legend("topright", max(ylim), paste(cond.names, "ms Gap"),
          col = col, pch = pch,
          lty = lty, lwd = 3, pt.bg = "white", pt.cex = 1.5,
          bty = "n", ncol = 2, y.intersp = 1.3, cex = .8);
}

do.fig0201();
rm(do.fig0201);
