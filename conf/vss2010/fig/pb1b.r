### pb1b.r: generates plot for ProbeTrack02 results

do.pb1b <- function () {
   rtfile <- "../../../probetrack02/data11.rda";
   dfile <- "../../../probetrack02/data02.rda";
   outfile <- "pb1b.pdf";
   thisfile <- "pb1b.r";
   exit.function <- function () {
      if (exists("opar")) par(opar);
      if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off();
   }
   on.exit(exit.function());

   source("common.r")

   ## hard code error values for RT and d'
   err.rt <- sqrt(1423 / 8) * qt(.975, 35);
   err.dp <- sqrt(0.2547 / 8) * qt(.975, 35);

   ## plotting limits
   ylim.max <- 825
   ylim.rt <- c(ylim.max - y.axis.range, ylim.max);
   ylim.dp <- c(0, 3);
   p.ylim.dp <- 1/3;
   showy.dp <- seq(ylim.dp[1], ylim.dp[2], by=1);
   ploty.dp <- (showy.dp - ylim.dp[1]) / diff(ylim.dp) * diff(ylim.rt) * p.ylim.dp + ylim.rt[1];
   at.ylab.dp <- mean(ploty.dp);
   err.dp <- err.dp / diff(ylim.dp) * diff(ylim.rt) * p.ylim.dp;

   ## load data files and rename the datasets
   load(rtfile);
   data.rt <- data11$data;
   fit.rt <- data11$fit;
   model.rt <- data11$model;
   load(dfile);
   data.dp <- data02;

   ## recode vars and filter out unused trials
   data.rt$soa <- as.numeric(as.character(data.rt$soa));
   data.rt <- data.rt[data.rt$gapdur != "0", , drop=TRUE];
   data.dp$soa <- as.numeric(as.character(data.dp$soa));
   data.dp <- data.dp[data.dp$gapdur != "0", , drop=TRUE];

   ## extract data to plot
   rt <- with(data.rt, tapply(rt, list(soa), mean));
   dp <- (with(data.dp, tapply(dprime, list(soa), mean)) - ylim.dp[1]) /
      diff(ylim.dp) * diff(ylim.rt) * p.ylim.dp + ylim.rt[1];
   showx <- as.numeric(dimnames(rt)[[1]]);
   plotx <- showx; plotx[plotx == 1280] <- scaling

   ## compute model for RT data
   Subjects <- sort(unique(as.character(data.rt$sub)));
   predx <- seq(min(plotx), max(plotx), by=1);
   rt.pred <- array(dim=c(length(Subjects), length(predx)),
                  dimnames=list(Subjects, predx));
   rtime <- fit.rt$rtime; names(rtime) <- fit.rt$sub;
   baseRT <- fit.rt$baseRT; names(baseRT) <- fit.rt$sub;
   for (sub in Subjects) {
      rt.pred[sub, ] <- model.rt(predx, rtime[sub], baseRT[sub]);
   }
   rt.pred <- apply(rt.pred, 2, mean);

   ## or just compute based on average parameters
   ##rt.pred <- model.rt(predx, 43.62047, 726.1516);

   ## open pdf file
   pdf(outfile, width=plotSize[1], height=plotSize[2], pointsize=fontSize)
   opar <- par(mfrow=c(1, 1), las=1, pty=pty, cex.axis=cex.axis,
               mar=mar, xpd=xpd, bg="white");

   ## prepare plotting area
   plot(plotx, rt, type="n", axes=F,
        ylim=ylim.rt, main="",
        xlab="Probe delay (ms)", ylab="Averaged median RT (ms)")
   axis(1, plotx, showx, lwd=lwd.axis, cex.axis=cex.axis)
   axis(2, seq(ylim.rt[1], ylim.rt[2], by=100),
        lwd=lwd.axis, cex.axis=cex.axis)
   axis(4, ploty.dp, showy.dp, lwd=lwd.axis, cex.axis=cex.axis)
   mtext("d'", side=4, line=2, las=0, at=at.ylab.dp)

   ## plot error bars and points
   lines(predx, rt.pred, lwd=lwd.model, col=1)
   arrows(plotx, rt - err.rt, plotx, rt + err.rt,
          length=.05, angle=90, code=3, lwd=lwd.ci, col=1, lty=1)
   points(plotx, rt, pch=pch.rt, col=1, bg="white", lwd=lwd.pts, cex=cex.pts)
   arrows(plotx, dp - err.dp, plotx, dp + err.dp,
          length=.05, angle=90, code=3, lwd=lwd.ci.dp, col=1, lty=1)
   lines(plotx, dp, type="o", bg="white",
         lwd=lwd.dp, lty=2, pch=pch.dp, cex=cex.pts.dp)

   ## add breaks to x-axis
   if (require("plotrix")) {
      axis.break(axis=1, breakpos=mean(c(160, scaling)), style="slash", brw=0.02);
   }
}

do.pb1b()
rm(do.pb1b)
