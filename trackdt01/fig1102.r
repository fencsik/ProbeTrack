### fig1102.r: plot fit of model against observed data, averaged across
### subjects

do.fig1102 <- function () {
   infile <- "data11.rda"
   outfile <- "fig1102.pdf"
   exit.function <- function () {
      if (exists("opar")) par(opar)
      if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off()
   }
   on.exit(exit.function())
   load(infile)
   model <- data11$model

   dt <- with(data11$data, tapply(rt, list(soa, cond), mean))

   ## gather parameters for model fits
   attach(data11$fit)
   factors <- list(sub, cond)
   rtime <- tapply(rtime, factors, mean)
   baseRT <- tapply(baseRT, factors, mean)
   detach()

   ## fit model to each subject in each condition
   Subjects <- dimnames(rtime)[[1]]
   predx <- seq(0, 400, by=1)
   predy <- array(dim=c(length(Subjects), length(predx), dim(rtime)[2]),
                  dimnames=list(Subjects, 1:length(predx), dimnames(rtime)[[2]]))
   for (sub in dimnames(predy)[[1]]) {
      for (gt in dimnames(predy)[[3]]) {
          predy[sub, , gt] <- model(predx, rtime[sub, gt], baseRT[sub, gt])
      }
   }
   predy <- apply(predy, 2:3, mean)
   baseRT <- apply(baseRT, 2, mean)

   x <- as.numeric(dimnames(dt)[[1]])
   condList <- dimnames(dt)[[2]]

   ## plot settings
   nCond <- length(condList)
   col <- rainbow(nCond)
   names(col) <- condList
   pch <- c(21, 23, 24)
   names(pch) <- condList

   pdf(outfile, width=6, height=6, pointsize=12)
   opar <- par(mfrow=c(1, 1), las=1, pty="m", cex.axis=.6,
               xpd=NA, bg="white")

   ylim <- c(350, 450)

   matplot(x, dt[, ], type="n", bty="n",
        axes=F, ylim=ylim,
        xlab="Probe delay (ms)", ylab="Probe RT (ms)", main="TrackDT01")
   for (gt in condList) {
      axis(1, x)
      axis(2)
      abline(h=baseRT[gt], xpd=F,
             col=col[gt], lwd=2, lty=3)
      lines(predx, predy[, gt], type="l",
            col=col[gt], lwd=2, lty=1)
      points(x, dt[, gt], type="p",
             col=col[gt], bg="transparent", pch=21, cex=1.5, lwd=3)
  }
}

do.fig1102()
rm(do.fig1102)
