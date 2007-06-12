### fig1201.r: plot fit of model against observed data for each subject
###
### $LastChangedDate$

do.fig1201 <- function () {
   infile <- "data12.rda";
   outfile <- "fig1201.pdf";
   thisfile <- "fig1201.r";
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
   model <- data12$model;
   fit <- data12$fit;
   dt <- with(data12$data, tapply(rt, list(soa, sub), mean));

   x <- as.numeric(dimnames(dt)[[1]]);
   subList <- dimnames(dt)[[2]];

   pdf(outfile, width = 18, height = 8, pointsize = 12);
   opar <- par(mfrow = c(2, 4), las = 1, pty = "m", cex.axis = .6,
               xpd = NA, bg = "white");

##    ## set up color matrix
##    col <- as.character(with(data12$data, tapply(rt, list(gapdur, ntargets), mean)));
##    col <- matrix(rainbow(length(gapdurList) * length(ntargetsList)),
##                  nrow = length(gapdurList), ncol = length(ntargetsList),
##                  dimnames = list(gapdurList, ntargetsList));
   ## other settings
   ylim.range <- 300;

   counter <- 0;
   for (sub in subList) {
      ylim <- c((mid <- mean(dt[, sub])) - ylim.range/2, mid + ylim.range/2);
      plot(x, dt[, sub], type = "n", bty = "n",
           axes = F, ylim = ylim,
           xlab = "", ylab = "", main = paste("ProbeTrack4", sub));
      axis(1, x);
      axis(2);
      if (counter %% 8 >= 3) {
         title(xlab = "Probe delay (ms)");
      }
      if (counter %% 4 == 0) {
         title(ylab = "Probe RT (ms)");
      }
      index <- rownames(fit) == sub;
      lines(i <- seq(min(x), max(x), by = .1),
            model(i, fit[index, "rtime"], fit[index, "baseRT"]),
            lty = 1, lwd = 3, col = 1);
      abline(h = fit[index, "baseRT"], xpd = F,
             lty = 2, col = 1, lwd = 2);
      points(x, dt[, sub],
             col = 1, bg = "white", pch = 21, cex = 1.5, lwd = 3);
      counter <- counter + 1;
   }
}

do.fig1201();
rm(do.fig1201);
