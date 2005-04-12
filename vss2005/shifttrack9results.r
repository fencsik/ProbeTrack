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

   col <- c(rgb(41, 51, 76, max=255), rgb(196, 229, 153, max=255))
   pt.col <- col
   pt.bg <- col
   pch <- c(24, 21)
   pt.cex <- 2
   line.lwd <- 6
   pt.lwd <- 1
   axis.lwd <- 4
   col.plot <- "black"#rgb(163, 195, 128, max=255)

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
   opar <- par(las=1, bty="o", pty="s", mar=c(5,4,2,2) + .1, xpd=NA,
               col.axis=col.plot, col.lab=col.plot)#, bg=rgb(128,141,255, max=255))

   x <- -1:1
   plot(x, dp[, 1], type="n", axes=F, ylim=c(.78,.88),
        xlab="reappearance position", ylab="")

   for (i in 1:2) {
      lines(x, dp[, i], col=col[i], lwd=line.lwd)
      points(x, dp[, i], pch=pch[i], col=pt.col[i], bg=pt.bg[i], lwd=pt.lwd, cex=pt.cex)
   }
   axis(1, x, c("rewind", "no-move", "move"), cex.axis=.8, lwd=axis.lwd, col=col.plot)
   axis(2, cex.axis=.8, lwd=axis.lwd, col=col.plot)

   legend(0, .81, c("simultaneous", "one-at-a-time"), bty="n", y.intersp=1.3,
          pch=pch, cex=.8, pt.lwd=pt.lwd, col=col, pt.bg = pt.bg, pt.cex=pt.cex)
}

do.shifttrack9results()
rm(do.shifttrack9results)
