### data20.r: analyze practice effects

do.data20 <- function () {
   infile <- "rawdata.rda"
   outfile.stem <- "data20"
   experiment <- "TrackDT02"
   summary.file <- sprintf("%s-summary.txt", outfile.stem)

   on.exit(while (sink.number() > 0) sink())
   on.exit(if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off(), add=TRUE)

   ## load data file and do some basic setup
   load(infile)
   dt <- alldata
   for (s in levels(factor(dt$sub))) {
       index <- dt$sub == s
       dt$trial[index] <- 1:sum(index)
   }

   ## output basic trial counts
   sink(summary.file)
   cat(experiment, "\n", sep="")
   cat("\nTotal trials per subject\n")
   print(with(dt, tapply(trial, list(sub), length)))
   cat("\nBad responses per subject\n")
   print(with(dt[dt$acc < 0, ], tapply(trial, list(sub), length)))
   cat("\nFast responses per subject\n")
   print(with(dt[dt$rt <= 50, ], tapply(trial, list(sub), length)))
   cat("\nSlow responses per subject\n")
   print(with(dt[dt$rt >= 5000, ], tapply(trial, list(sub), length)))
   sink()

   ## remove problematic trials (bad responses and fast/slow responses)
   dt <- dt[dt$acc >= 0 & dt$rt > 50 & dt$rt < 5000, ]

   ## output accuracy
   sink(summary.file, append=TRUE)
   cat("\nAverage accuracy per subject\n")
   print(with(dt, tapply(acc, list(sub), mean)), digits=3)
   sink()

   ## plot rt across trials
   pdf(sprintf("%s-loess.pdf", outfile.stem), width=11, height=8.5, pointsize=12)
   par(mfrow=c(2, 2), las=1, pty="m", bg="white")
   for (s in levels(factor(dt$sub))) {
       dp <- dt[dt$sub == s, ]
       plot(x <- dp$trial, y <- dp$rt,
            main=sprintf("%s - Subject %s", experiment, s),
            xlab="Trial", ylab="Reaction time (ms)",
            bty="l", ylim=c(400, 2000))
       lines(x, predict(loess(y ~ x)), col="red", lwd=2)
   }
   dev.off()

   ## split trials into groups
   groups <- c(0, 20, 40)
   dt2 <- dt[dt$trial <= max(groups), ]
   dt2$trial.quant <- cut(dt2$trial, groups, labels=1:(length(groups) - 1))
   dt3.rt <- with(dt2, tapply(rt, list(sub, trial.quant), mean))
   dt3.acc <- with(dt2, tapply(acc, list(sub, trial.quant), mean))
   x <- as.numeric(dimnames(dt3.rt)[[2]])

   ## output # of trials per split per subject
   sink(summary.file, append=TRUE)
   cat("\nAverage accuracy per subject per bin\n")
   print(with(dt2, tapply(trial, list(sub, trial.quant), length)))
   sink()

   color.rt <- "blue"
   color.acc <- "red"

   pdf(sprintf("%s-quart.pdf", outfile.stem), width=11, height=8.5, pointsize=12)
   par(mfrow=c(2, 2), las=1, pty="m", bg="white",
       mar=c(5, 4, 4, 4) + 1.1)
   for (s in levels(factor(dt$sub))) {
       plot(x, dt3.rt[s, ], type="o", lwd=2, col=color.rt,
            main=sprintf("%s - Subject %s", experiment, s),
            xlab="Trial bin", ylab="",
            ylim=c(500, 1200),
            bty="n", axes=F)
       axis(1, 1:4)
       axis(2, col=color.rt, labels=F)
       mtext("Reaction time (ms)", side=2, line=3, las=0, cex=.8, col=color.rt)
       at <- axTicks(2)
       mtext(side=2, text=at, at=at, col=color.rt, line=1, cex=.7)
       par(new=T, las=1)
       plot(x, dt3.acc[s, ], type="o", lwd=2, col=color.acc,
            ylim=c(0.5, 1.0),
            main="", xlab="", ylab="", axes=F)
       axis(4, col=color.acc, labels=F)
       at <- axTicks(4)
       mtext(side=4, text=at, at=at, col=color.acc, line=1, cex=.7)
       mtext("Proportion correct", side=4, line=2.5, las=0, cex=.8, col=color.acc)
   }
   dev.off()
   pdf(sprintf("%s-quart-avg.pdf", outfile.stem), width=6, height=5, pointsize=12)
   par(las=1, pty="m", bg="white",
       mar=c(5, 4, 4, 4) + 1.1)
   plot(x, apply(dt3.rt, 2, mean), type="o", lwd=2, col=color.rt,
        main=sprintf("%s - Average", experiment),
        xlab="Trial bin", ylab="",
        ylim=c(500, 1200),
        bty="n", axes=F)
   axis(1, 1:4)
   axis(2, col=color.rt, labels=F)
   mtext("Reaction time (ms)", side=2, line=3, las=0, cex=.8, col=color.rt)
   at <- axTicks(2)
   mtext(side=2, text=at, at=at, col=color.rt, line=1, cex=.7)
   par(new=T, las=1)
   plot(x, apply(dt3.acc, 2, mean), type="o", lwd=2, col=color.acc,
        ylim=c(0.5, 1.0),
        main="", xlab="", ylab="", axes=F)
   axis(4, col=color.acc, labels=F)
   at <- axTicks(4)
   mtext(side=4, text=at, at=at, col=color.acc, line=1, cex=.7)
   mtext("Proportion correct", side=4, line=2.5, las=0, cex=.8, col=color.acc)
   dev.off()

   ## prepare for ANOVAs of rt as a function of trial bin
   dt4.rt <- as.data.frame(as.table(dt3.rt))
   names(dt4.rt) <- c("sub", "bin", "rt")
   dt4.acc <- as.data.frame(as.table(dt3.acc))
   names(dt4.acc) <- c("sub", "bin", "acc")

   ## generate and output ANOVAs
   sink(summary.file, append=T)
   cat("\nANOVA on RT as a function of trial bin\n")
   print(summary(aov(rt ~ bin + Error(sub / bin), dt4.rt)))
   cat("\nANOVA on accuracy as a function of trial bin\n")
   print(summary(aov(acc ~ bin + Error(sub / bin), dt4.acc)))
   sink()
}

print(system.time(do.data20(), TRUE))
rm(do.data20)
