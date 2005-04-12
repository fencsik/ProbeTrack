### vss2005/stoptrack4results.r: plot stoptrack 3 and 4 results for VSS 2005
###
### $LastChangedDate$

do.stoptrackresults <- function() {
   infile3 <- "../stoptrack3/data01out.rda"
   infile4 <- "../stoptrack4/data01out.rda"
   pdffile <- "stoptrack4results.pdf"
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

   if (!file.exists(infile3)) stop(infile3, " not found.")
   load(infile3)
   data01$sub <- as.character(data01$sub)
   data01$movetype <- as.character(data01$movetype)
   data01$ntargets <- as.numeric(as.character(data01$ntargets))
   stoptrack3 <- data01

   if (!file.exists(infile4)) stop(infile4, " not found.")
   load(infile4)
   data01$sub <- as.character(data01$sub)
   data01$movetype <- as.character(data01$movetype)
   data01$ntargets <- as.numeric(as.character(data01$ntargets))
   stoptrack4 <- data01

   dt <- rbind(stoptrack3, stoptrack4)
   dp <- tapply(dt$pcor, list(dt$ntargets, dt$movetype), mean)

   pdf(file = pdffile, width = 8, height = 8, horiz = F, family = "Helvetica", pointsize = 30)
   opar <- par(las=1, pty="s", bty="n", mar=c(5,4,2,2) + .1, xpd=T,
               col.axis=col.plot, col.lab=col.plot)

   x <- as.numeric(dimnames(dp)[[1]])
   matplot(x, dp, type="n", axes=F, xlim=c(2,4), ylim=c(.6, 1),
           xlab="number of targets", ylab="")

   axis(1, seq(2, max(x), by=1), cex.axis=.8, lwd=axis.lwd, col=col.plot)
   axis(2, seq(.6, 1, by=.1), cex.axis=.8, lwd=axis.lwd, col=col.plot)
   for (i in 2:1) {
      lines(x, dp[, i], col=col[i], lwd=line.lwd)
      points(x, dp[, i], pch=pch[i], col=pt.col[i], bg=pt.bg[i], lwd=pt.lwd, cex=pt.cex)
   }

   ##text(3, .96, "moving", cex=1)
   ##text(3, .79, "static", cex=1)

   legend(3, 1, c("moving", "static"), bty="n", y.intersp=1.3,
          pch=pch, cex=.8, pt.lwd=pt.lwd, col=col, pt.bg = pt.bg, pt.cex=pt.cex)
}

do.stoptrackresults()
rm(do.stoptrackresults)
