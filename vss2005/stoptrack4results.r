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

   col <- rep("black", 2)
   pt.bg <- rgb(163, 195, 128, max=255)
   pch <- c(22, 23)
   pt.cex <- .8
   line.lwd <- 6
   pt.lwd <- 3
   axis.lwd <- 4

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
   opar <- par(las=1, pty="s", bty="n", mar=c(5,4,2,2) + .1, xpd=T)

   x <- as.numeric(dimnames(dp)[[1]])
   matplot(x, dp, type="n", axes=F, ylim=c(.6, 1),
           xlab="Number of Targets", ylab="Proportion Correct")

   axis(1, x, cex.axis=.8, lwd=axis.lwd)
   axis(2, cex.axis=.8, lwd=axis.lwd)
   for (i in 2:1) {
      lines(x, dp[, i], col=col[i], lwd=line.lwd)
      points(x, dp[, i], pch=pch[i], col=col[i], bg=pt.bg, lwd=pt.lwd, pt.lwd=1, cex=pt.cex)
   }

   ##text(3, .96, "moving", cex=1)
   ##text(3, .79, "static", cex=1)

   legend(3, 1, c("Moving", "Static"), bty="n", y.intersp=1.3,
          pch=pch, cex=.8, pt.lwd=pt.lwd, col=col, pt.bg = pt.bg, pt.cex=pt.cex)
}

do.stoptrackresults()
rm(do.stoptrackresults)
