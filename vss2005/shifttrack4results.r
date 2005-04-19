### vss2005/shifttrack4results.r: plot shifttrack4 accuracy VSS2005 poster
###
### $LastChangedDate$

do.shifttrack4results <- function() {
   infile <- "../shifttrack4/data01out.rda"
   pdffile <- "shifttrack4results.pdf"
   exit.function <- function () {
      if (exists("opar")) par(opar)
      if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off()
   }
   on.exit(exit.function())

   col <- c(rgb(41, 51, 76, max=255), rgb(196, 229, 153, max=255))
     ##c(rgb(65, 76, 51, max=255), rgb(196, 229, 153, max=255))
   pt.col <- col
   pt.bg <- col #rgb(163, 195, 128, max=255)
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
   data01$ntargets <- as.numeric(as.character(data01$ntargets))

   dp <- tapply(data01$pcor, list(data01$shift, data01$ntargets), mean)
   ci <- function(x) qt(.95, length(x) - 1) * sqrt(var(x)/length(x))
   sem <- function(x) sqrt(var(x) / length(x))
   ## dps <- tapply(data01$pcor, list(data01$shift), sem)
   ## dps <- .002

   pdf(file = pdffile, width = 8, height = 8, horiz=F, family = "Helvetica", pointsize = 30)
   opar <- par(las=1, pty="s", bty="n", mar=c(5,4,2,2) + .1, xpd=NA, mgp=c(2.5, 0.8, 0.3),
               col.axis=col.plot, col.lab=col.plot)#, bg=rgb(128,141,255, max=255))

   x <- -1:1
   plot(x, dp[, 1], type="n", axes=F, ylim=c(.6,1),
        xlab="reappearance position", ylab="")

   for (i in 1:2) {
      lines(x, dp[, i], col=col[i], lwd=line.lwd)
      points(x, dp[, i], pch=pch[i], col=pt.col[i], bg=pt.bg[i], lwd=pt.lwd, cex=pt.cex)
   }   
   axis(1, x, c("rewind", "no-move", "move"), cex.axis=.8, lwd=axis.lwd, col=col.plot)
   axis(2, cex.axis=.8, lwd=axis.lwd, col=col.plot)

   legend(.4, 1.05, c("2 targets", "5 targets"), bty="n", y.intersp=1.3, text.col=col.plot,
          pch=pch, cex=.8, pt.lwd=pt.lwd, col=pt.col, pt.bg = pt.bg, pt.cex=pt.cex)
}

do.shifttrack4results()
rm(do.shifttrack4results)
