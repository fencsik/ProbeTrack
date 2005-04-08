### vss2005/shifttrack9results.r: plot shifttrack9 accuracy VSS2005 poster
###
### $LastChangedDate$

do.shifttrack9results <- function() {
   infile <- "../shifttrack9/data01out.rda"
   pdffile <- "shifttrack9results.pdf"
   exit.function <- function () {
      if (exists("opar")) par(opar)
      if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off()
   }
   on.exit(exit.function())

   col <- rep("black", 2)
   pt.bg <- rgb(163, 195, 128, max=255)
   pch <- c(24, 21)
   pt.cex <- .8
   line.lwd <- 6
   pt.lwd <- 3
   axis.lwd <- 4

   if(!file.exists(infile))  stop("cannot open file ", infile)
   load(infile)

   ## tweak data
   data01$shift <- as.numeric(as.character(data01$shift))
   data01$asynch <- as.numeric(as.character(data01$asynch))

   dp <- tapply(data01$pcor, list(data01$shift, data01$asynch), mean)
   print(dp)
   ci <- function(x) qt(.95, length(x) - 1) * sqrt(var(x)/length(x))
   sem <- function(x) sqrt(var(x) / length(x))
   ## dps <- tapply(data01$pcor, list(data01$shift), sem)
   ## dps <- .002

   pdf(file = pdffile, width = 8, height = 8, horiz=F, family = "Helvetica", pointsize = 30)
   opar <- par(las=1, bty="o", pty="s", mar=c(5,4,2,2) + .1, xpd=T)#, bg=rgb(128,141,255, max=255))

   x <- -1:1
   plot(x, dp[, 1], type="n", axes=F, ylim=c(.78,.88),
        xlab="Reappearance Position", ylab="Proportion Correct")

   lines(x, dp[, 1], col=col[1], lwd=line.lwd)
   lines(x, dp[, 2], col=col[2], lwd=line.lwd)
   points(x, dp[, 1], pch=pch[1], col=col[1], bg=pt.bg, lwd=pt.lwd, pt.lwd=1, cex=pt.cex)
   points(x, dp[, 2], pch=pch[2], col=col[2], bg=pt.bg, lwd=pt.lwd, cex=pt.cex)
   
   axis(1, x, c("Rewind", "No-Move", "Move"), cex.axis=.8, lwd=axis.lwd)
   axis(2, cex.axis=.8, lwd=axis.lwd)

   legend(-.25, .82, c("Simultaneous", "One-At-A-Time"), bty="n", y.intersp=1.3,
          pch=pch, cex=.8, pt.lwd=pt.lwd, col=col, pt.bg = pt.bg, pt.cex=pt.cex)
}

do.shifttrack9results()
rm(do.shifttrack9results)
